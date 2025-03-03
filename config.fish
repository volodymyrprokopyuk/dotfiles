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
  cp base/starship.toml ~/.config
end

function Emacs -a install
  cp base/*.el ~/.config/doom
  if test -n "$install"
    set root ~/.config/emacs
    set backup ~/.config/emacs2
    rm -rf $backup
    mv $root $backup
    git clone --depth 1 https://github.com/doomemacs/doomemacs $root
    cd $root; and ./bin/doom install; and ./bin/doom sync; and cd -
    # doom upgrade
  end
end

function Git
  cp base/sshconfig ~/.ssh/config
  source base/gitconfig
end

function I3wm
  set host (hostname)
  awk "{ sub(/^# $host: /, \"\"); print }" i3wm/i3config > ~/.config/i3/config
  cp i3wm/i3status.toml ~/.config/i3status-rust/config.toml
  cp i3wm/roficonfig.rasi ~/.config/rofi/config.rasi
end

function Zathura
  cp apps/zathurarc ~/.config/zathura
end

function Lilypond -a lpversion
  if not string match -qr '^(?<long>(?<short>\d\.\d{1,2})\.\d{1,2})$' $lpversion
    echo argparse: missing or invalid LilyPond version; and return 1
  end
  set url https://lilypond.org/download/sources/v$short/lilypond-$long.tar.gz
  set root ~/.config/lilypond
  set build $root/lilypond-$long/build
  curl -fsSL $url | tar xvz -C $root; and mkdir $build; and cd $build
  and ../autogen.sh --noconfigure
  and ../configure --prefix=$root --disable-documentation GUILE_FLAVOR=guile-3.0
  and make -j 4; and make install
end

function Foundry -a fversion
  if test -z "$fversion"
    echo argparse: missing Foundry version; and return 1
  end
  set root ~/.config/foundry
  mkdir -p $root/bin
  set release https://github.com/foundry-rs/foundry/releases/download
  set url $release/$fversion/foundry_stable_linux_amd64.tar.gz
  curl -fsSL $url | tar xvz -C $root/bin
  forge completions fish > ~/.config/fish/completions/forge.fish
  cast completions fish > ~/.config/fish/completions/cast.fish
  anvil completions fish > ~/.config/fish/completions/anvil.fish
end

argparse --min-args=1 init a/all i/install v/version= -- $argv; or return

if set -ql _flag_init
  mkdir -p ~/.config/{wezterm,fish,doom,i3,i3status-rust,rofi} \
    ~/.config/{zathura,lilypond,foundry} ~/.ssh
end

if set -ql _flag_all
  Wezterm; Fish; Nushell; Emacs; Git; I3wm; Zathura; and return
end

for target in $argv
  switch $target
  case wezterm; Wezterm
  case fish; Fish
  case nushell; Nushell
  case emacs; Emacs $_flag_install
  case git; Git
  case i3wm; I3wm
  case zathura; Zathura
  case lilypond; Lilypond $_flag_version
  case foundry; Foundry $_flag_version
  case '*'
    echo argparse: unknown target $target; and return 1
  end
end
