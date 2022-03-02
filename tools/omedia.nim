#[
omedia organizes photo and video files into a directory tree by date
Usage: omedia -i:<sourceDir> -o:<sinkDir>
Dependencies
- exiv2 extracts date and time from a photo file
- ffprobe (from ffmpeg) extracts date and time from a video file
]#

import std/[strformat, strutils, sequtils]
import std/[parseopt, threadpool, os, osproc, times, sha1]
import pkg/regex

type
  MediaType = enum mtImage, mtVideo
  Media = object
    mType: MediaType
    source: string
    tStamp: DateTime
    digest: string
  OptionsError = object of ValueError
  FSError = object of OSError

const
  reImageSource = re"\.(?i:jpe?g)"
  cmdImageTStamp = "exiv2 -K Exif.Image.DateTime -P v"
  reImageTStamp = re"(?P<tStamp>\d{4}:\d{2}:\d{2} \d{2}:\d{2}:\d{2})"
  fmtImageTStamp = "yyyy:MM:dd HH:mm:ss"
  reVideoSource = re"\.(?i:mp4|mov|avi)"
  cmdVideoTStamp = "ffprobe -v quiet -select_streams v:0 -show_entries stream_tags=creation_time -of default=nw=1:nk=1"
  reVideoTStamp = re"(?P<tStamp>\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})"
  fmtVideoTStamp = "yyyy-MM-dd'T'HH:mm:ss"

proc parseOptions(): tuple[input, output: string] =
  var input, output: string
  for _, key, value in getOpt():
    case key:
    of "i", "input": input = value
    of "o", "output": output = value
  if input == "" or output == "":
    raise newException(OptionsError, "options -i and -o are mandatory")
  if not dirExists(input):
    raise newException(FSError, "input directory does not exist: " & input)
  createDir(output)
  (input, output)

iterator walkMedia(sourceDir: string,
                   reImage = reImageSource,
                   reVideo = reVideoSource): Media =
  for file in walkDirRec(sourceDir):
    if file.endsWith(reImage):
      yield Media(mType: mtImage, source: file)
    elif file.endsWith(reVideo):
      yield Media(mType: mtVideo, source: file)
    else: stderr.write(fmt "WARNING: unknown media type: {file}\n")

proc extractImageTStamp(media: var Media,
                        lastMediaTStamp: var DateTime,
                        log: var seq[string],
                        cmdTStamp = cmdImageTStamp,
                        reTStamp = reImageTStamp,
                        fmtTStamp = fmtImageTStamp) =
  let (output, exitCode) =
    execCmdEx(fmt "{cmdTStamp} {quoteShell media.source}")
  if exitCode != 0:
    log.add(fmt "WARNING: exiv2: non-zero exit code: {exitCode}")
  var m: RegexMatch
  if output.find(reTStamp, m):
    media.tStamp = m.groupFirstCapture("tStamp", output).parse(fmtTStamp)
    lastMediaTStamp = media.tStamp
  else:
    media.tStamp = lastMediaTStamp

proc extractVideoTStamp(media: var Media,
                        lastMediaTStamp: var DateTime,
                        log: var seq[string],
                        cmdTStamp = cmdVideoTStamp,
                        reTStamp = reVideoTStamp,
                        fmtTStamp = fmtVideoTStamp) =
  let (output, exitCode) =
    execCmdEx(fmt "{cmdTStamp} {quoteShell media.source}")
  if exitCode != 0:
    log.add(fmt "WARNING: ffprobe: non-zero exit code: {exitCode}")
  var m: RegexMatch
  if output.find(reTStamp, m):
    media.tStamp = m.groupFirstCapture("tStamp", output).parse(fmtTStamp)
    lastMediaTStamp = media.tStamp
  else:
    media.tStamp = lastMediaTStamp

proc digestMedia(media: var Media) =
  media.digest = media.source.secureHashFile.`$`.toLowerAscii

proc writeMedia(media: Media, sinkDir: string) =
  let mediaDir = media.tStamp.format("yyyy/yyyy-MM-dd")
  let mediaExt = media.source.splitFile[2].toLowerAscii.replace(re"jpeg$", "jpg")
  let mediaFile = media.tStamp.format("yyyyMMdd'_'HHmmss'_'") &
    media.digest & mediaExt
  createDir(sinkDir / mediaDir)
  copyFile(media.source, sinkDir / mediaDir / mediaFile)

proc organizeMedia(mediaGroup: seq[Media], sinkDir: string) =
  var lastMediaTStamp = now()
  for media in mediaGroup:
    var
      media = media
      log = newSeq[string](3)
    try:
      case media.mType:
      of mtImage:
        log.add(fmt "IMAGE: {media.source}")
        extractImageTStamp(media, lastMediaTStamp, log)
      of mtVideo:
        log.add(fmt "VIDEO: {media.source}")
        extractVideoTStamp(media, lastMediaTStamp, log)
      digestMedia(media)
      writeMedia(media, sinkDir)
    except CatchableError as error:
      log.add(fmt "ERROR: {error.msg}")
    echo log.join("\n").strip

try:
  let
    (inputDir, outputDir) = parseOptions()
    cpuCores = countProcessors()
    mediaGroups = inputDir.walkMedia.toSeq.distribute(cpuCores)
  for mediaGroup in mediaGroups:
    spawn organizeMedia(mediaGroup, outputDir)
  sync()
except OptionsError as error:
  stderr.write fmt "ERROR: {error.msg}\n"
  quit("Usage: omedia -i:<sourceDir> -o:<sinkDir>")
