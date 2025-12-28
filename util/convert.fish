#!/usr/bin/env fish

for img in source/*.jpg
  echo $img
  magick $img -interlace Plane -sampling-factor 4:2:0 -quality 80 sink/(basename $img)
end
