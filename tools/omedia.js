#!/usr/bin/env node

/*
 * omedia organizes photo and video files into a directory tree by date
 * Usage: omedia -i <sourceDir> -o <sinkDir> -j <jobs>
 * Dependencies
 * - exiv2 extracts date and time from a photo file
 * - ffprobe (from ffmpeg) extracts date and time from a video file
 * cp -r ~/Media/sink/2022/* / /run/media/vlad/SD2/Media-2021-202_/2022
 */

import { cpus } from "os"
import chalk from "chalk"
import parseArgs from "minimist"
import { pathExists } from "fs-extra"
import { globbyStream } from "globby"
import { $ } from "zx"

let lastTstamp = null

export function asyncParallel(jobs, limit) {
  let index = 0
  let running = 0
  let completed = 0
  return new Promise((resolve, reject) => {
    function done(error) {
      if (error) { console.error(error) }
      if (++completed === jobs.length) { return resolve() }
      if (--running < limit) { parallel() }
    }
    function parallel() {
      while (index < jobs.length && running < limit) {
        const job = jobs[index++]
        job().finally(done)
        ++running
      }
    }
    parallel()
  })
}

function configure() {
  const argsConfig = {
    alias: { i: "input", o: "output", j: "jobs" },
    default: { i: ".", o: "sink", j: cpus().length }
  }
  return parseArgs(process.argv.slice(2), argsConfig)
}

async function organizeImage(file) {
  const logs = []
  logs.push(`${chalk.green("IMAGE:")} ${file}`)
  const tstamp = await extractImageTsmapt(file)
  if (!tstamp) { logs.push("__") }
  const digest = await digestMedia(file)
  await writeMedia(file, tstamp, digest)
  console.log(logs.join("\n"))
}

async function organizeVideo(file) {
  const logs = []
  logs.push(`${chalk.blue("VIDEO:")} ${file}`)
  console.log(logs.join("\n"))
}

async function readMedia(args) {
  const jobs = []
  if (!await pathExists(args.i)) {
    throw new Error(`Input directory ${args.i} does not exist`)
  }
  for await (const file of globbyStream(args.i)) {
    if (/(jpe?g|heic)$/i.test(file)) {
      jobs.push(() => organizeImage(file))
    } else if (/(mp4|mov|avi)$/i.test(file)) {
      jobs.push(() => organizeVideo(file))
    } else {
      console.log(chalk.red("UNKNOWN:"), file)
    }
  }
  return jobs
}

async function organizeMedia(args) {
  const jobs = await readMedia(args)
  return await asyncParallel(jobs, args.j)
}

await organizeMedia(configure())
