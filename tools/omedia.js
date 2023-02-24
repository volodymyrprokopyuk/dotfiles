#!/usr/bin/env node

/*
 * omedia organizes photo and video files into a directory tree by date
 * Usage: omedia -i <sourceDir> -o <sinkDir> -j <jobs>
 * Dependencies
 * - exiv2 extracts date and time from a photo file
 * - ffprobe (from ffmpeg) extracts date and time from a video file
 * cp -r ~/Media/sink/2022/* / /run/media/vlad/SD2/Media-2021-202_/2022
 */

import { DateTime as dt } from "luxon"
import { extname } from "path"
import { cpus } from "os"
import { readFile, copyFile } from "fs/promises"
import { pathExists, mkdirp } from "fs-extra"
import { globbyStream } from "globby"
import { createHash } from "crypto"
import chalk from "chalk"
import parseArgs from "minimist"
import { $ } from "zx"

$.verbose = false
let lastTs = dt.now()

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
    const output = await fromReadable(res.stdout)
    return dt.fromFormat(output, "yyyy:MM:dd HH:mm:ss\n")
  } catch (error) { logs.push(chalk.red(error)) }
}

async function digestMedia(file) {
  const hash = createHash("sha256")
  hash.update(await readFile(file))
  return hash.digest("hex").slice(0, 16)
}

async function writeMedia(file, ts, digest, args) {
  const dir = ts.toFormat("yyyy/yyyy-MM-dd")
  const fileTime = ts.toFormat("yyyyMMdd_HHmmss")
  const ext = extname(file).toLowerCase().replace("jpeg", "jpg")
  await mkdirp(`${args.o}/${dir}`)
  await copyFile(file, `${args.o}/${dir}/${fileTime}_${digest}${ext}`)
}

async function organizeMedia(file, type, args) {
  const logs = []
  try {
    let ts
    switch (type) {
      case "image": {
        logs.push(`${chalk.green("IMAGE:")} ${file}`)
        ts = await extractImageTs(file, logs)
        break
      }
      case "video": {
        logs.push(`${chalk.blue("VIDEO:")} ${file}`)
        ts = await extractImageTs(file, logs)
        break
      }
    }
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
      jobs.push(() => organizeMedia(file, "image", args))
    } else if (/(mp4|mov|avi)$/i.test(file)) {
      jobs.push(() => organizeMedia(file, "video", args))
    } else {
      console.log(chalk.yellow("UNKNOWN:"), file)
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
    default: { i: ".", o: "sink", j: cpus().length }
  }
  return parseArgs(process.argv.slice(2), argsConfig)
}

await organize(configure())
