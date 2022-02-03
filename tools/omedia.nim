import std/[strutils, strformat, parseopt, os, osproc, times, sha1], pkg/regex

type
  MediaType = enum mtImage, mtVideo
  Media = object
    mType: MediaType
    source: string
    tStamp: DateTime
    digest: string

const
  reImageSource = re"\.(?i:jpe?g)"
  cmdImageTStamp = "exiv2 -K Exif.Image.DateTime -P v"
  reImageTStamp = re"(?P<tStamp>\d{4}:\d{2}:\d{2} \d{2}:\d{2}:\d{2})"
  fmtImageTStamp = "yyyy:MM:dd HH:mm:ss"
  reVideoSource = re"\.(?i:mp4|mov|avi)"
  cmdVideoTStamp = "ffprobe -v quiet -select_streams v:0 -show_entries stream_tags=creation_time -of default=nw=1:nk=1"
  reVideoTStamp = re"(?P<tStamp>\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})"
  fmtVideoTStamp = "yyyy-MM-dd'T'HH:mm:ss"

var
  lastMediaTStamp = now()

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

iterator walkMedia(sourceDir: string,
                   reImage = reImageSource,
                   reVideo = reVideoSource): Media =
  for file in walkDirRec sourceDir:
    if file.endsWith(reImage):
      yield Media(mType: mtImage, source: file)
    elif file.endsWith(reVideo):
      yield Media(mType: mtVideo, source: file)
    else: stderr.write fmt "WARNING: unknown media type: {file}\n"

proc extractImageTStamp(media: var Media,
                        cmdTStamp = cmdImageTStamp,
                        reTStamp = reImageTStamp,
                        fmtTStamp = fmtImageTStamp) =
  let (output, exitCode) =
    execCmdEx fmt "{cmdTStamp} {quoteShell media.source}"
  if exitCode != 0:
    stderr.write fmt "WARNING: exiv2: non-zero exit code: {exitCode}\n"
  var m: RegexMatch
  if output.find(reTStamp, m):
    media.tStamp = m.groupFirstCapture("tStamp", output).parse(fmtTStamp)
    lastMediaTStamp = media.tStamp
  else:
    media.tStamp = lastMediaTStamp

proc extractVideoTStamp(media: var Media,
                        cmdTStamp = cmdVideoTStamp,
                        reTStamp = reVideoTStamp,
                        fmtTStamp = fmtVideoTStamp) =
  let (output, exitCode) =
    execCmdEx fmt "{cmdTStamp} {quoteShell media.source}"
  if exitCode != 0:
    stderr.write fmt "WARNING: ffprobe: non-zero exit code: {exitCode}\n"
  var m: RegexMatch
  if output.find(reTStamp, m):
    media.tStamp = m.groupFirstCapture("tStamp", output).parse(fmtTStamp)
    lastMediaTStamp = media.tStamp
  else:
    media.tStamp = lastMediaTStamp

proc digestMedia(media: var Media) =
  media.digest = toLowerAscii $secureHashFile media.source

proc writeMedia(media: Media, sinkDir: string) =
  proc extractExt(source: string): string =
    var m: RegexMatch
    if source.find(re"(?P<ext>\.\w{2,4})$", m):
      m.groupFirstCapture("ext", source).toLowerAscii.replace(re"jpeg$", "jpg")
    else:
      $media.mType
  let mediaDir = media.tStamp.format("yyyy/yyyy-MM-dd")
  let mediaExt = extractExt media.source
  let mediaFile = media.tStamp.format("yyyyMMdd'_'HHmmss'_'") &
    media.digest & mediaExt
  createDir fmt "{sinkDir}/{mediaDir}"
  copyFile media.source, fmt "{sinkDir}/{mediaDir}/{mediaFile}"

proc organizeMedia(sourceDir, sinkDir: string) =
  for media in walkMedia(sourceDir):
    var media = media
    try:
      case media.mType:
      of mtImage:
        echo fmt "IMAGE: {media.source}"
        extractImageTStamp media
      of mtVideo:
        echo fmt "VIDEO: {media.source}"
        extractVideoTStamp media
      digestMedia media
      writeMedia media, sinkDir
    except CatchableError as error:
      stderr.write fmt "ERROR: {error[]}\n"

let (inputDir, outputDir) = parseOptions()
organizeMedia inputDir, outputDir
