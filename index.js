#!/usr/bin/env node

import { promisify } from "util"
import { pipeline as pipelineCb } from "stream"
const pipeline = promisify(pipelineCb)
import { homedir } from "os"
import { createWriteStream } from "fs"
import { copyFile, writeFile } from "fs/promises"
import { mkdirp, remove, ensureFile } from "fs-extra"
import { argv, $, cd, fetch } from "zx"
import tar from "tar"

$.verbose = false
const xdgHome = `${homedir()}/.config`
const rawGitHub = "https://raw.githubusercontent.com"

async function configDir(dir) {
  const path = `${xdgHome}/${dir}`
  await mkdirp(path)
  return path
}

async function get(url, path) {
  const { body } = await fetch(url)
  return await pipeline(body, createWriteStream(path))
}

async function wezterm() {
  const path = await configDir("wezterm")
  await copyFile("base/wezterm.lua", `${path}/wezterm.lua`)
}

async function zsh() {
  const path = await configDir("zsh")
  await copyFile("base/.zshrc", `${path}/.zshrc`)
  await copyFile("base/starship.toml", `${xdgHome}/starship.toml`)
}

async function git() {
  const path = await configDir("git")
  await remove(`${path}/config`)
  await ensureFile(`${path}/config`)
  await $`git config --global user.name "Volodymyr Prokopyuk"`
  await $`git config --global user.email "volodymyrprokopyuk@gmail.com"`
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
  await writeFile(`${path}/ignore`, gitignore.join("\n"))
}

async function base() {
  const path = await configDir("bat")
  await copyFile("base/batconfig", `${path}/config`)
}

async function emacs() {
  const path = await configDir("doom")
  for (const file of ["packages.el", "init.el", "config.el"]) {
    await copyFile(`base/${file}`, `${path}/${file}`)
  }
}

async function lilypond() {
  const path = `${xdgHome}/doom/lilypond`
  await mkdirp(path)
  const url = `${rawGitHub}/lilypond/lilypond/master/elisp`
  const files = [
    "lilypond-font-lock.el", "lilypond-indent.el", "lilypond-init.el",
    "lilypond-mode.el", "lilypond-song.el", "lilypond-what-beat.el"
  ]
  return await Promise.all(
    files.map(file => get(`${url}/${file}`, `${path}/${file}`))
  )
}

async function installLilypond(v) {
  // yay -S fontforge t1utils extractpdfmark pdfcpu
  // yay -S dblatex tex-gyre-fonts texlive-langcyrillic
  const home = `${homedir()}/.lilypond`
  const url = `https://lilypond.org/download/source/v${v.replace(/\.\d+$/, "")}`
  const version = `lilypond-${v}`, archive = `${version}.tar.gz`
  await mkdirp(home); cd(home)
  await get(`${url}/${archive}`, `${home}/${archive}`)
  await tar.extract({ file: `${home}/${archive}` })
  cd (version); await mkdirp("build"); cd ("build"); $.verbose = true
  await $`../autogen.sh --noconfigure`
  await $`../configure --prefix=${home} --disable-documentation \
    GUILE_FLAVOR=guile-3.0`
  await $`make -j4`; await $`make install`
}

async function app() {
  const path = await configDir("zathura")
  await copyFile("app/zathurarc", `${path}/zathurarc`)
}

for (const arg of argv._) {
  switch (arg) {
    case "wezterm": await wezterm(); break
    case "zsh": await zsh(); break
    case "git": await git(); break
    case "base": await base(); break
    case "emacs": await emacs(); break
    case "lilypond": await lilypond(); break
    case "ililypond": await installLilypond("2.25.4"); break
    case "app": await app(); break
    case "all":
      await Promise.all(
        [wezterm(), zsh(), git(), base(), emacs(), lilypond(), app()]
      ); break
    default: console.log(`WARNING: unknown option ${arg}`)
  }
}