import std/[strformat, parseopt, os, osproc], pkg/regex

type
  MediaType = enum mtImage, mtVideo
  Media = object
    mType: MediaType
    source: string
    # tstamp: times
    digest: string
    sink: string

const
  reImageSource = re"(?i:jpe?g)"
  cmdImageTstamp = "exiv2 -K Exif.Image.DateTime -P v"
  reImageTstamp = re"(?P<y>\d{4}):(?P<m>\d{2}):(?P<d>\d{2}) (?P<H>\d{2}):(?P<M>\d{2}):(?P<S>\d{2})"
  reVideoSource = re"(?i:mp4|mov|avi)"
  cmdVideoTstamp = "ffprobe -v quiet -select_streams v:0 -show_entries stream_tags=creation_time -of default=nw=1:nk=1"
  reVideoTstamp = re"(?P<y>\d{4})-(?P<m>\d{2})-(?P<d>\d{2})T(?P<H>\d{2}):(?P<M>\d{2}):(?P<S>\d{2})"

iterator walkMedia(sourceDir: string,
                   reImage = reImageSource,
                   reVideo = reVideoSource): Media =
  for file in walkDirRec sourceDir:
    if file.endsWith(reImage):
      yield Media(mType: mtImage, source: file)
    elif file.endsWith(reVideo):
      yield Media(mType: mtVideo, source: file)
    else: stderr.write fmt "WARNING: unknown media type: {file}\n"

proc parseOptions(): tuple[input, output: string] =
  var
    input = getCurrentDir()
    output = input
  for _, key, value in getopt():
    case key:
    of "i", "input": input = value
    of "o", "output": output = value
  if not dirExists input:
    raise newException(OSError, fmt "input directory does not exist: {input}")
  createDir output
  (input, output)

proc extractImageTstamp(media: var Media,
                        cmdTstamp = cmdImageTstamp,
                        reTstamp = reImageTstamp) =
  let (output, exitCode) =
    execCmdEx fmt "{cmdTstamp} {quoteShell media.source}"
  if exitCode != 0:
    stderr.write fmt "WARNING: exiv2: non-zero exit code: {exitCode}\n"
  var m: RegexMatch
  if output.find(reTstamp, m):
    echo output, m.groupFirstCapture("m", output)
  else:
    stderr.write fmt "WARNING: exiv2: no timestamp\n"

proc extractVideoTstamp(media: var Media,
                        cmdTstamp = cmdVideoTstamp,
                        reTstamp = reVideoTstamp) =
  let (output, exitCode) =
    execCmdEx fmt "{cmdTstamp} {quoteShell media.source}"
  if exitCode != 0:
    stderr.write fmt "WARNING: ffprobe: non-zero exit code: {exitCode}\n"
  var m: RegexMatch
  if output.find(reTstamp, m):
    echo output, m.groupFirstCapture("m", output)
  else:
    stderr.write fmt "WARNING: ffprobe: no timestamp\n"

proc organizeMedia(sourceDir, sinkDir: string) =
  for media in walkMedia(sourceDir):
    var media = media
    case media.mType:
    of mtImage:
      echo fmt "IMAGE: {media.source}"
      extractImageTstamp media
    of mtVideo:
      echo fmt "VIDEO: {media.source}"
      extractVideoTstamp media

let (inputDir, outputDir) = parseOptions()
organizeMedia inputDir, outputDir
