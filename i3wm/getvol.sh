busctl --user call rs.i3status $1 rs.i3status.custom SetText ss Volume \
  $(wpctl get-volume $2 | awk '{ if ($3) print "mute"; else print $2 * 100 }')
