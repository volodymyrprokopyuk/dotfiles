#!/usr/bin/env node

import { argv, $, os, fs, fetch } from "zx"
import { createWriteStream } from "fs"
import { copyFile, writeFile } from "fs/promises"
const home = os.homedir()
const rawGitHub = "https://raw.githubusercontent.com"

async function GET(url, path) {
  const { body } = await fetch(url)
  const ws = createWriteStream(path)
  return new Promise((resolve, reject) => {
    body.on("error", e => { fs.remove(path); reject(e) })
    body.pipe(ws)
    ws.on("finish", () => { ws.close(); resolve() })
  })
}

async function git() {
  await fs.remove(`${home}/.gitconfig`)
  await $`git config --global user.name "Volodymyr Prokopyuk"`
  await $`git config --global user.email "volodymyrprokopyuk@gmail.com"`
  await $`git config --global core.excludesfile "~/.gitignore"`
  await $`git config --global init.defaultBranch "master"`
  await $`git config --global push.default "simple"`
  await $`git config --global pull.rebase false`
  await $`git config --global diff.algorithm "histogram"`
  await $`git config --global status.showUntrackedFiles "all"`
  await $`git config --global credential.helper "cache --timeout=86400"`
  await $`git config --global core.quotepath "false"`
  await $`git config --global core.pager "delta"`
  await $`git config --global delta.syntax-theme "1337"`
  await $`git config --global delta.minus-style 'syntax "#480000"'`
  await $`git config --global delta.minus-emph-style 'syntax "#8F0000"'`
  await $`git config --global delta.plus-style 'syntax "#002C00"'`
  await $`git config --global delta.plus-emph-style 'syntax "#006700"'`
  await $`git config --global alias.s "!git status -sb && git stash list"`
  await $`git config --global alias.d "diff"`
  await $`git config --global alias.ds "diff --staged"`
  await $`git config --global alias.l 'log --all --graph --abbrev-commit --pretty=format:"%C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)"'`
  await $`git config --global alias.lf 'log -p -M --follow --abbrev-commit --pretty=format:"%C(bold blue)commit%C(reset) %C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)\\n"'`
  await $`git config --global alias.lsf 'log -p -M --follow --abbrev-commit --pretty=format:"%C(bold blue)commit%C(reset) %C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)\\n" -S'`
  await $`git config --global alias.lgf 'log -p -M --follow --abbrev-commit --pretty=format:"%C(bold blue)commit%C(reset) %C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)\\n" -G'`
  await $`git config --global alias.bb "branch -a -vv"`
  await $`git config --global alias.bm "branch --merged"`
  await $`git config --global alias.bnm "branch --no-merged"`
  await $`git config --global alias.ch "checkout"`
  await $`git config --global alias.a "!git add -A && git s"`
  await $`git config --global alias.cm "!git diff --check && git commit"`
  const gitignore = ["*~", "node_modules"]
  await writeFile(`${home}/.gitignore`, gitignore.join("\n"))
}

async function wezterm() {
  const path = `${home}/.config/wezterm`
  await fs.mkdirp(path)
  await copyFile("wezterm.lua", `${path}/wezterm.lua`)
}

async function zsh() {
  await copyFile(".zshrc", `${home}/.zshrc`)
  const path = `${home}/.config`
  await fs.mkdirp(path)
  await copyFile("starship.toml", `${path}/starship.toml`)
}

async function emacs() {
  await fs.mkdirp(`${home}/.doom.d`)
  for (const file of ["packages.el", "init.el", "config.el"]) {
    await copyFile(file, `${home}/.doom.d/${file}`)
  }
}

async function lilypond() {
  const path = `${home}/.doom.d/lilypond`
  await fs.mkdirp(path)
  const url = `${rawGitHub}/lilypond/lilypond/master/elisp`
  const files = [
    "lilypond-font-lock.el", "lilypond-indent.el", "lilypond-init.el",
    "lilypond-mode.el", "lilypond-song.el", "lilypond-what-beat.el"
  ]
  Promise.all(files.map(file => GET(`${url}/${file}`, `${path}/${file}`)))
}

async function zathura() {
  const path = `${home}/.config/zathura`
  await fs.mkdirp(path)
  await copyFile("zathurarc", `${path}/zathurarc`)
}

async function tools() {
  // bat
  const path = `${home}/.config/bat`
  await fs.mkdirp(path)
  await copyFile("batconfig", `${path}/config`)
  // psql
  await copyFile(".psqlrc", `${home}/.psqlrc`)
}

for (const arg of argv._) {
  switch (arg) {
    case "git": await git(); break
    case "wezterm": await wezterm(); break
    case "zsh": await zsh(); break
    case "emacs": await emacs(); break
    case "lilypond": await lilypond(); break
    case "zathura": await zathura(); break
    case "tools": await tools(); break
    case "all":
      await Promise.all(
        [git(), wezterm(), zsh(), emacs(), lilypond(), zathura(), tools()]
      ); break
    default: console.log(`WARNING: unknown option ${arg}`)
  }
}
