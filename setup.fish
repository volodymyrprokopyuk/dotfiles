#!/usr/bin/env fish

argparse --min-args=1 init v/version= -- $argv; or return

if set -ql _flag_init
  echo $_flag_init
end

if contains all $argv
  echo all; and return
end

for target in $argv
  switch $target
  case wezterm
    echo wezterm
  case fish
    echo fish
  case emacs
    echo emacs
  case git
    echo git
  case i3wm
    echo i3wm
  case apps
    echo apps
  case lpond
    echo lpond
  case '*'
    echo argparse: unknown target $target; and return 1
  end
end
