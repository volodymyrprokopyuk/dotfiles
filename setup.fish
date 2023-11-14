#!/usr/bin/env fish

function Wezterm
  cp base/wezterm.lua ~/.config/wezterm
end

function Fish
  cp base/config.fish ~/.config/fish
  cp base/starship.toml ~/.config
end

function Emacs
  cp base/*.el ~/.config/doom
end

function Git
  cp base/sshconfig ~/.ssh/config
  echo "" > ~/.config/git/config
  source base/gitconfig
  set -l gitignore '*~'\nnode_modules/\ncoverage/\nartifacts/\ncache/
  echo -e $gitignore > ~/.config/git/ignore
end

function I3wm
  set -l host (hostname)
  cp i3wm/i3config ~/.config/i3/config
  sd "^# $host: " '' ~/.config/i3/config
  cp i3wm/i3status.toml ~/.config/i3status-rust/config.toml
  cp i3wm/roficonfig.rasi ~/.config/rofi/config.rasi
end

function Apps
  cp apps/zathurarc ~/.config/zathura
end

function Lpond
  if not string match -qr '^(?<ver>(?<v>\d\.\d{1,2})\.\d{1,2})$' $argv[1]
    echo argparse: missing or invalid LilyPond version; and return 1
  end
  set -l url https://lilypond.org/download/sources/v$v/lilypond-$ver.tar.gz
  set -l root ~/.config/lilypond; set -l dir $root/lilypond-$ver/build
  curl -fsSL $url | tar xvz -C $root; and mkdir $dir; and cd $dir
  and ../autogen.sh --noconfigure
  and ../configure --prefix=$root --disable-documentation \
    GUILE_FLAVOR=guile-3.0
  and make -j 4; and make install
end

argparse --min-args=1 init v/version= -- $argv; or return

if set -ql _flag_init
  mkdir -p ~/.config/{wezterm,fish,doom,git,i3,i3status-rust,rofi} \
    ~/.config/{zathura,lilypond} ~/.ssh
end

if contains all $argv
  Wezterm; Fish; Emacs; Git; I3wm; Apps; and return
end

for target in $argv
  switch $target
  case wezterm; Wezterm
  case fish; Fish
  case emacs; Emacs
  case git; Git
  case i3wm; I3wm
  case apps; Apps
  case lpond; Lpond $_flag_version
  case '*'
    echo argparse: unknown target $target; and return 1
  end
end
