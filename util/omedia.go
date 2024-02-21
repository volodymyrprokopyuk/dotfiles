package main

import (
  "fmt"
  "flag"
  "regexp"
  "io/fs"
  "path/filepath"
  "os"
)

var inDir, outDir string

func parseArgs() error {
  flag.StringVar(&outDir, "o", "", "output directory")
  flag.Parse()
  args := flag.Args()
  if len(args) != 1 {
    return fmt.Errorf("exactly one input directory expected, got %v", args)
  }
  inDir = args[0]
  if len(outDir) == 0 {
    return fmt.Errorf("missing output directory")
  }
  return nil
}

type organizer interface {
  organize(outDir string) error
}

type image struct {
  path string
}

func (img image) organize(outDir string) error {
  fmt.Println(img)
  return nil
}

type video struct {
  path string
}

func (vid video) organize(outDir string) error {
  fmt.Println(vid)
  return nil
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
        media = append(media, image{path})
      case reVideo.MatchString(path):
        media = append(media, video{path})
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
  err := parseArgs()
  if err != nil {
    fmt.Println(err)
    os.Exit(1)
  }
  media, err := readMedia(inDir)
  if err != nil {
    fmt.Println(err)
    os.Exit(1)
  }
  for _, med := range media {
    err := med.organize(outDir)
    if err != nil {
      fmt.Println(err)
    }
  }
}
