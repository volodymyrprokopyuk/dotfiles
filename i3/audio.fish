set device $argv[1]
set target $argv[2]
set volume (wpctl get-volume $device)
set pattern 'Volume: (?<value>\d\.\d{2})(?<muted> \[MUTED\])?'
string match --quiet --regex $pattern $volume
set message (test -n "$muted" && echo mute || echo (math $value \* 100))
busctl --user call rs.i3status $target rs.i3status.custom SetText ss Volume $message
