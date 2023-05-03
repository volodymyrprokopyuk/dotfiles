#!/usr/bin/env node

/*
 * omedia organizes photo and video files into a directory tree by date
 * Usage: omedia -i <sourceDir> -o <sinkDir> -j <jobs>
 * Dependencies
 * - exiv2 extracts date and time from a photo file
 * - ffprobe (from ffmpeg) extracts date and time from a video file
 * - openssl generates a digest of a media file
 * cp -r ~/Media/sink/2022/* / /run/media/vlad/SD2/Media-2021-202_/2022
 */

import { DateTime } from "luxon"
import { extname } from "path"
import { copyFile } from "fs/promises"
import { pathExists, mkdirp } from "fs-extra"
import { globbyStream } from "globby"
import chalk from "chalk"
import parseArgs from "minimist"
import { $ } from "zx"

$.verbose = false
let lastTs = DateTime.now()

function parseTs(str, format) {
  const ts = DateTime.fromFormat(str, format)
  if (ts.invalid) {
    throw new Error(ts.invalid.explanation)
  }
  return ts
}

async function fromReadable(readable) {
  let res = ""
  for await (const chunk of readable) { res += chunk }
  return res
}

function asyncParallel(jobs, limit) {
  let index = 0
  let running = 0
  let completed = 0
  return new Promise((resolve, reject) => {
    function done(error) {
      if (error) { console.error(chalk.red(error)) }
      if (++completed === jobs.length) { return resolve() }
      if (--running < limit) { parallel() }
    }
    function parallel() {
      while (index < jobs.length && running < limit) {
        const job = jobs[index++]
        job().then(done, done)
        ++running
      }
    }
    parallel()
  })
}

async function extractImageTs(file, logs) {
  try {
    const res = await $`exiv2 -K Exif.Image.DateTime -P v ${file}`
    let output = await fromReadable(res.stdout)
    output = output.replace("\n", "")
    return parseTs(output, "yyyy:MM:dd HH:mm:ss")
  } catch (error) { logs.push(chalk.red(error)) }
}

async function extractVideoTs(file, logs) {
  try {
    const res = await $`ffprobe -v quiet -select_streams v:0 -show_entries stream_tags=creation_time -of default=nw=1:nk=1 ${file}`
    let output = await fromReadable(res.stdout)
    output = output.replace(/\.\d+Z$\n/m, "")
    return parseTs(output, "yyyy-MM-dd'T'HH:mm:ss")
  } catch(error) { logs.push(chalk.red(error)) }
}

async function digestMedia(file) {
  const res = await $`openssl dgst -sha256 ${file}`
  const output = await fromReadable(res.stdout)
  return output.replace(/^.+= /, "").slice(0, 16)
}

async function writeMedia(file, ts, digest, args) {
  const dir = ts.toFormat("yyyy/yyyy-MM-dd")
  const fileTime = ts.toFormat("yyyyMMdd_HHmmss")
  const ext = extname(file).toLowerCase().replace("jpeg", "jpg")
  await mkdirp(`${args.o}/${dir}`)
  await copyFile(file, `${args.o}/${dir}/${fileTime}_${digest}${ext}`)
}

async function organizeMedia(file, type, extractTs, args) {
  const logs = []
  try {
    logs.push(`${chalk.green(type)} ${file}`)
    let ts = await extractTs(file, logs)
    if (ts) { lastTs = ts } else { ts = lastTs }
    const digest = await digestMedia(file)
    await writeMedia(file, ts, digest, args)
  } catch (error) { logs.push(chalk.red(error)) }
  console.log(logs.join("\n"))
}

async function readMedia(args) {
  const jobs = []
  if (!await pathExists(args.i)) {
    throw new Error(`Input directory ${args.i} does not exist`)
  }
  for await (const file of globbyStream(args.i)) {
    if (/(jpe?g|heic)$/i.test(file)) {
      jobs.push(() => organizeMedia(file, "image", extractImageTs, args))
    } else if (/(mp4|mov|avi)$/i.test(file)) {
      jobs.push(() => organizeMedia(file, "video", extractVideoTs, args))
    } else {
      console.log(chalk.yellow("unknown"), file)
    }
  }
  return jobs
}

async function organize(args) {
  const jobs = await readMedia(args)
  return await asyncParallel(jobs, args.j)
}

function configure() {
  const argsConfig = {
    alias: { i: "input", o: "output", j: "jobs" },
    default: { i: ".", o: "sink", j: 128 }
  }
  return parseArgs(process.argv.slice(2), argsConfig)
}

await organize(configure())
