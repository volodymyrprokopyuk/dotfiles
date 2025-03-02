#!/usr/bin/env fish
# -*- mode: fish -*-

function Wezterm
  cp base/wezterm.lua ~/.config/wezterm
end

function Fish
  cp base/config.fish ~/.config/fish
  cp base/starship.toml ~/.config
end

function Nushell
  cp base/*.nu ~/.config/nushell
end

function Emacs -a install
  cp base/*.el ~/.config/doom
  set dir ~/.config/emacs
  # doom sync # config
  if test -n "$install"
    rm -rf {$dir}{1,2}
    git clone --depth 1 https://github.com/doomemacs/doomemacs {$dir}2
    cd {$dir}2; and ./bin/doom install # packages
      and mv $dir {$dir}1; and mv {$dir}2 $dir; and doom sync; and cd -
    # doom upgrade # Doom and packages
  end
end

function Git
  cp base/sshconfig ~/.ssh/config
  echo "" > ~/.config/git/config
  source base/gitconfig
  set gitignore '*~'\nnode_modules/\ncoverage/\ncoverage.cov
  echo -e $gitignore > ~/.config/git/ignore
end

function I3wm
  set host (hostname)
  awk "{ sub(/^# $host: /, \"\"); print }" i3wm/i3config > ~/.config/i3/config
  cp i3wm/i3status.toml ~/.config/i3status-rust/config.toml
  cp i3wm/roficonfig.rasi ~/.config/rofi/config.rasi
end

function Apps
  cp apps/zathurarc ~/.config/zathura
end

function Lpond -a lpversion
  if not string match -qr '^(?<ver>(?<v>\d\.\d{1,2})\.\d{1,2})$' $lpversion
    echo argparse: missing or invalid LilyPond version; and return 1
  end
  set url https://lilypond.org/download/sources/v$v/lilypond-$ver.tar.gz
  set root ~/.config/lilypond
  set dir $root/lilypond-$ver/build
  curl -fsSL $url | tar xvz -C $root; and mkdir $dir; and cd $dir
  and ../autogen.sh --noconfigure
  and ../configure --prefix=$root --disable-documentation \
    GUILE_FLAVOR=guile-3.0
  and make -j 4; and make install
end

function Foundry -a fversion
  if test -z "$fversion"
    echo argparse: missing Foundry version; and return 1
  end
  set dir ~/.config/foundry
  mkdir -p $dir/bin
  set release https://github.com/foundry-rs/foundry/releases/download
  set url $release/$fversion/foundry_nightly_linux_amd64.tar.gz
  curl -fsSL $url | tar xvz -C $dir/bin
  forge completions fish > ~/.config/fish/completions/forge.fish
  cast completions fish > ~/.config/fish/completions/cast.fish
  anvil completions fish > ~/.config/fish/completions/anvil.fish
end

argparse --min-args=1 init i/install v/version= -- $argv; or return

if set -ql _flag_init
  mkdir -p ~/.config/{wezterm,fish,doom,git,i3,i3status-rust,rofi} \
    ~/.config/{zathura,lilypond,foundry} ~/.ssh
end

if contains all $argv
  Wezterm; Fish; Nushell; Emacs; Git; I3wm; Apps; and return
end

for target in $argv
  switch $target
  case wezterm; Wezterm
  case fish; Fish
  case nushell; Nushell
  case emacs; Emacs $_flag_install
  case git; Git
  case i3wm; I3wm
  case apps; Apps
  case lpond; Lpond $_flag_version
  case foundry; Foundry $_flag_version
  case '*'
    echo argparse: unknown target $target; and return 1
  end
end
