#!/usr/bin/env node

import { promisify } from "util"
import { pipeline as pipelineCb } from "stream"
const pipeline = promisify(pipelineCb)
import { homedir } from "os"
import { createWriteStream } from "fs"
import { copyFile, writeFile } from "fs/promises"
import { mkdirp, remove } from "fs-extra"
import fetch from "node-fetch"
import { argv, $ } from "zx"

$.verbose = false
const home = homedir()
const rawGitHub = "https://raw.githubusercontent.com"

async function GET(url, path) {
  const { body } = await fetch(url)
  return await pipeline(body, createWriteStream(path))
}

async function git() {
  await remove(`${home}/.gitconfig`)
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
  await mkdirp(path)
  await copyFile("base/wezterm.lua", `${path}/wezterm.lua`)
}

async function zsh() {
  await copyFile("base/.zshrc", `${home}/.zshrc`)
  const path = `${home}/.config`
  await mkdirp(path)
  await copyFile("base/starship.toml", `${path}/starship.toml`)
}

async function emacs() {
  await mkdirp(`${home}/.doom.d`)
  for (const file of ["packages.el", "init.el", "config.el"]) {
    await copyFile(`base/${file}`, `${home}/.doom.d/${file}`)
  }
}

async function lilypond() {
  const path = `${home}/.doom.d/lilypond`
  await mkdirp(path)
  const url = `${rawGitHub}/lilypond/lilypond/master/elisp`
  const files = [
    "lilypond-font-lock.el", "lilypond-indent.el", "lilypond-init.el",
    "lilypond-mode.el", "lilypond-song.el", "lilypond-what-beat.el"
  ]
  return await Promise.all(
    files.map(file => GET(`${url}/${file}`, `${path}/${file}`))
  )
}

async function zathura() {
  const path = `${home}/.config/zathura`
  await mkdirp(path)
  await copyFile("base/zathurarc", `${path}/zathurarc`)
}

async function util() {
  const path = `${home}/.config/bat`
  await mkdirp(path)
  await copyFile("base/batconfig", `${path}/config`)
}

for (const arg of argv._) {
  switch (arg) {
    case "git": await git(); break
    case "wezterm": await wezterm(); break
    case "zsh": await zsh(); break
    case "emacs": await emacs(); break
    case "lilypond": await lilypond(); break
    case "zathura": await zathura(); break
    case "util": await util(); break
    case "all":
      await Promise.all(
        [git(), wezterm(), zsh(), emacs(), lilypond(), zathura(), util()]
      ); break
    default: console.log(`WARNING: unknown option ${arg}`)
  }
}
