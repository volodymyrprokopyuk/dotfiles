#!/usr/bin/env nu
# -*- mode: nushell -*-

def "main wezterm" [] {
  cp base/wezterm.lua ~/.config/wezterm
}

def "main nushell" [] {
  cp base/*.nu ~/.config/nushell
  cp base/starship.toml ~/.config
}

def "main fish" [] {
  cp base/config.fish ~/.config/fish
  cp base/starship.toml ~/.config
}

def "main emacs" [--install (-i)] {
  cp base/*.el ~/.config/doom
  if $install {
    let dir = ("~/.config/emacs" | path expand)
    let backup = ("~/.config/emacs2" | path expand)
    if ($dir | path exists) {
      rm --verbose --recursive --force $backup
      mv --verbose $dir $backup
      git clone --depth 1 https://github.com/doomemacs/doomemacs $dir
      doom install
      doom sync
      # doom upgrade
    }
  }
}

def "main git" [] {
  cp base/sshconfig ~/.ssh/config
  source base/gitconfig
}

def "main i3" [] {
  let host = $"# (hostname): "
  "i3/i3config" | open | str replace --all $host "" | save -f ~/.config/i3/config
  cp i3/i3status.toml ~/.config/i3status-rust/config.toml
  cp i3/roficonfig.rasi ~/.config/rofi/config.rasi
}

def "main zathura" [] {
  cp base/zathurarc ~/.config/zathura
}

def "main lilypond" [--version (-v): string] {
  let v = ($version | parse --regex '(?P<long>(?P<short>\d.\d{1,2}).\d{1,2})')
  if ($v | is-empty) {
    print $"error: missing or invalid version: ($version)"
    exit 1
  }
  let release = $"https://lilypond.org/download/sources/v($v.short.0)"
  let archive = $"lilypond-($v.long.0).tar.gz"
  let dir = ("~/.config/lilypond" | path expand)
  curl -fsSL $"($release)/($archive)" | tar xvz -C $dir
  let build = $"($dir)/lilypond-($v.long.0)/build"
  mkdir $build; cd $build
  ../autogen.sh --noconfigure
  ../configure $"--prefix=($dir)" --disable-documentation GUILE_FLAVOR=guile-3.0
  make --jobs=4; make install
}

def "main foundry" [--version (-v): string = "stable"] {
  if ($version | is-empty) {
    print "error: missing version"
    exit 1
  }
  let dir = ("~/.config/foundry/bin" | path expand)
  let release = "https://github.com/foundry-rs/foundry/releases/download"
  let archive = "foundry_stable_linux_amd64.tar.gz"
  curl -fsSL $"($release)/($version)/($archive)" | tar xvz -C $dir
}

def main [--init (-i), --all (-a)] {
  if $init {
    let dirs = [
      wezterm, nushell, fish, doom, i3, i3status-rust, rofi,
      zathura, lilypond, foundry
    ]
    mkdir ...($dirs | each { $"~/.config/($in)" | path expand }) ~/.ssh
  }
  if $all {
    main wezterm
    main nushell
    main fish
    main emacs
    main git
    main i3
    main zathura
  }
}
