/*
 * omedia organizes photo and video files into a directory tree by date
 * Usage: omedia -o <outDir> <inDir>
 * Dependencies
 * - exiv2 extracts date and time from an image file
 * - ffprobe (from ffmpeg) extracts date and time from a video file
 * rclone copy --progress lanagph:media/all ~/Media/source/
 * ./omedia -o ~/Media/sink ~/Media/source
 * sudo mkdir /run/media/SD{1,2,3,4}
 * sudo chown -R vlad:vlad /run/media/SD{1,2,3,4}
 * sudo mount -a
 * cp -rv ~/Media/sink/2022/* /run/media/vlad/SD1/Media-2021-202_/2022
 * sudo umount /run/media/SD{1,2,3,4}
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
  "crypto/sha256"
  "sync"
  "os"
  "os/exec"
)

type organizeCommand struct {
  inDir, outDir string
}

func parseArgs() (organizeCommand, error) {
  var outDir string
  flag.StringVar(&outDir, "o", "", "output directory")
  flag.Parse()
  if len(outDir) == 0 {
    return organizeCommand{},
      fmt.Errorf("missing output directory")
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

func copyFile(src, dst string) (int64, error) {
  srcFile, err := os.Open(src)
  if err != nil {
    return 0, err
  }
  defer srcFile.Close()
  dstFile, err := os.Create(dst)
  if err != nil {
    return 0, err
  }
  defer dstFile.Close()
  return io.Copy(dstFile, srcFile)
}

func writeMedia(
  inFile string, tstamp time.Time, digest, outDir string,
) error {
  outDir = filepath.Join(outDir, tstamp.Format("2006/2006-01-02"))
  outFile := fmt.Sprintf(
    "%v_%v%v",
    tstamp.Format("20060102_150405"), digest[:8],
    strings.Replace(strings.ToLower(filepath.Ext(inFile)), ".jpeg", ".jpg", 1),
  )
  err := os.MkdirAll(outDir, 0755)
  if err != nil {
    return err
  }
  _, err = copyFile(inFile, filepath.Join(outDir, outFile))
  if err != nil {
    return err
  }
  return nil
}

type organizer interface {
  organize(outDir string, lastTs *time.Time) error
}

var cutTime, _ = time.Parse("2006-01-02", "2000-01-01")

type image string

func extractImageTs(file string) (time.Time, error) {
  out, err := exec.Command(
    "exiv2", "-K", "Exif.Image.DateTime", "-P", "v", file,
  ).Output()
  if err != nil {
    return time.Time{}, err
  }
  tstamp, err := time.Parse(
    "2006:01:02 15:04:05", strings.TrimRight(string(out), "\n"),
  )
  if err != nil {
    return time.Time{}, err
  }
  return tstamp, nil
}

func (img image) organize(outDir string, lastTs *time.Time) error {
  file := string(img)
  tstamp, err := extractImageTs(file)
  if err != nil {
    fmt.Printf("error: %v %v\n", file, err)
    tstamp = *lastTs
  }
  if (tstamp.Before(cutTime)) {
    tstamp = *lastTs
  }
  *lastTs = tstamp
  digest, err := digestMedia(file)
  if err != nil {
    return err
  }
  return writeMedia(file, tstamp, digest, outDir)
}

type video string

func extractVideoTs(file string) (time.Time, error) {
  out, err := exec.Command(
    "ffprobe", "-v", "quiet", "-select_streams", "v:0", "-show_entries",
    "stream_tags=creation_time", "-of", "default=nw=1:nk=1", file,
  ).Output()
  if err != nil {
    return time.Time{}, err
  }
  tstamp, err := time.Parse(
    "2006-01-02T15:04:05", strings.TrimSuffix(string(out), ".000000Z\n"),
  )
  if err != nil {
    return time.Time{}, err
  }
  return tstamp, nil
}

func (vid video) organize(outDir string, lastTs *time.Time) error {
  file := string(vid)
  tstamp, err := extractVideoTs(file)
  if err != nil {
    fmt.Printf("error: %v %v\n", file, err)
    tstamp = *lastTs
  }
  if (tstamp.Before(cutTime)) {
    tstamp = *lastTs
  }
  *lastTs = tstamp
  digest, err := digestMedia(file)
  if err != nil {
    return err
  }
  return writeMedia(file, tstamp, digest, outDir)
}

var (
  reImage = regexp.MustCompile(`(?i:jpe?g|webp|heic)$`)
  reVideo = regexp.MustCompile(`(?i:mp4|mov|avi)$`)
)

func readMedia(inDir string) ([]organizer, error) {
  files := make([]organizer, 0, 1e3)
  walk := func(path string, de fs.DirEntry, err error) error {
    if err != nil {
      return err
    }
    if (!de.IsDir()) {
      switch {
      case reImage.MatchString(path):
        files = append(files, image(path))
      case reVideo.MatchString(path):
        files = append(files, video(path))
      default:
        fmt.Printf("unknown file: %v\n", path)
      }
    }
    return nil
  }
  err := filepath.WalkDir(inDir, walk)
  if err != nil {
    return nil, err
  }
  return files, nil
}

func organizeMedia(files []organizer, oc organizeCommand) {
  var wg sync.WaitGroup
  in := make(chan organizer)
  for range min(len(files), 8) {
    wg.Add(1)
    go func() {
      defer wg.Done()
      var lastTs = time.Now()
      for file := range in {
        fmt.Println(file)
        err := file.organize(oc.outDir, &lastTs)
        if err != nil {
          fmt.Printf("error: %v\n", err)
        }
      }
    }()
  }
  for _, file := range files {
    in <- file
  }
  close(in)
  wg.Wait()
}

func main() {
  oc, err := parseArgs()
  if err != nil {
    fmt.Printf("error: %v\n", err)
    os.Exit(1)
  }
  files, err := readMedia(oc.inDir)
  if err != nil {
    fmt.Printf("error: %v\n", err)
    os.Exit(1)
  }
  organizeMedia(files, oc)
}
