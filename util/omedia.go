/*
 * omedia organizes photo and video files into a directory tree by date
 * Usage: omedia -o <outDir> <inDir>
 * Dependencies
 * - exiv2 extracts date and time from an image file
 * - ffprobe (from ffmpeg) extracts date and time from a video file
 * cp -r ~/Media/sink/2022/* /run/media/vlad/SD1/Media-2021-202_/2022
 */
package main

import (
  "fmt"
  "strings"
  "flag"
  "regexp"
  "time"
  "io"
  "io/fs"
  "path/filepath"
  "os"
  "os/exec"
  "crypto/sha256"
)

var lastTs = time.Now()

type organizeCommand struct {
  inDir, outDir string
}

func parseArgs() (organizeCommand, error) {
  var outDir string
  flag.StringVar(&outDir, "o", "", "output directory")
  flag.Parse()
  if len(outDir) == 0 {
    return organizeCommand{}, fmt.Errorf("missing output directory")
  }
  _, err := os.Stat(outDir)
  if os.IsNotExist(err) {
    return organizeCommand{},
      fmt.Errorf("output directory %v does not exist", outDir)
  }
  args := flag.Args()
  if len(args) != 1 {
    return organizeCommand{},
      fmt.Errorf("exactly one input directory expected, got %v", args)
  }
  return organizeCommand{inDir: args[0], outDir: outDir}, nil
}

func digestMedia(file string) (string, error) {
  f, err := os.Open(file)
  if err != nil {
    return "", err
  }
  defer f.Close()
  hash := sha256.New()
  _, err = io.Copy(hash, f)
  if err != nil {
    return "", err
  }
  return fmt.Sprintf("%x", hash.Sum(nil)), nil
}

func writeMedia(file string, tstamp time.Time, digest string) error {
  dir := tstamp.Format("2006/2006-01-01")
  file = fmt.Sprintf(
    "%v_%v%v",
    tstamp.Format("20060102_150405"), digest[:8],
    strings.Replace(strings.ToLower(filepath.Ext(file)), ".jpeg", ".jpg", 1),
  )
  fmt.Printf("%v/%v\n", dir, file)
  return nil
}

type organizer interface {
  organize(outDir string) error
}

type image string

func (img image) organize(outDir string) error {
  file := string(img)
  out, err := exec.Command(
    "exiv2", "-K", "Exif.Image.DateTime", "-P", "v", file,
  ).Output()
  if err != nil {
    return err
  }
  tstamp, err := time.Parse(
    "2006:01:02 15:04:05", strings.TrimRight(string(out), "\n"),
  )
  if err != nil {
    return err
  }
  digest, err := digestMedia(file)
  if err != nil {
    return err
  }
  return writeMedia(file, tstamp, digest)
}

type video string

func (vid video) organize(outDir string) error {
  file := string(vid)
  out, err := exec.Command(
    "ffprobe", "-v", "quiet", "-select_streams", "v:0", "-show_entries",
    "stream_tags=creation_time", "-of", "default=nw=1:nk=1", file,
  ).Output()
  if err != nil {
    return err
  }
  tstamp, err := time.Parse(
    "2006-01-02T15:04:05", strings.TrimSuffix(string(out), ".000000Z\n"),
  )
  if err != nil {
    return err
  }
  digest, err := digestMedia(file)
  if err != nil {
    return err
  }
  return writeMedia(file, tstamp, digest)
}

var (
  reImage = regexp.MustCompile(`(?i:jpe?g|webp|heic)$`)
  reVideo = regexp.MustCompile(`(?i:mp4|mov|avi)$`)
)

func readMedia(inDir string) ([]organizer, error) {
  media := make([]organizer, 0, 1e3)
  err := filepath.WalkDir(inDir, func(
    path string, de fs.DirEntry, err error,
  ) error {
    if err != nil {
      return err
    }
    if (!de.IsDir()) {
      switch {
      case reImage.MatchString(path):
        media = append(media, image(path))
      case reVideo.MatchString(path):
        media = append(media, video(path))
      default:
        fmt.Printf("unknown file: %v\n", path)
      }
    }
    return nil
  })
  if err != nil {
    return nil, err
  }
  return media, nil
}

func main() {
  oc, err := parseArgs()
  if err != nil {
    fmt.Printf("error: %v\n", err)
    os.Exit(1)
  }
  media, err := readMedia(oc.inDir)
  if err != nil {
    fmt.Printf("error: %v\n", err)
    os.Exit(1)
  }
  for _, med := range media {
    err := med.organize(oc.outDir)
    if err != nil {
      fmt.Printf("error: %v\n", err)
    }
  }
}
