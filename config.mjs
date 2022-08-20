#!/usr/bin/env node
import { promisify } from "util"
import { exec as execCb } from "child_process"
const exec = promisify(execCb)
import { existsSync, createWriteStream } from "fs"
import { mkdir, copyFile, writeFile, unlink } from "fs/promises"
import { homedir } from "os"
const home = homedir()
import { get as GET } from "https"
const rawGitHub = "https://raw.githubusercontent.com"

async function removeFile(file) {
  if (existsSync(file)) { await unlink(file) }
}

async function download(url, path) {
  const ws = createWriteStream(path)
  return new Promise((resolve, reject) => {
    GET(url, res => {
      res.pipe(ws)
      ws.on("finish", () => { ws.close(); resolve() })
    }).on("error", e => { unlink(path); reject(e) })
  })
}

async function git() {
  const cmds = `git config --global user.name "Volodymyr Prokopyuk"
    git config --global user.email "volodymyrprokopyuk@gmail.com"
    git config --global core.excludesfile "~/.gitignore"
    git config --global init.defaultBranch "master"
    git config --global push.default "simple"
    git config --global pull.rebase false
    git config --global diff.algorithm "histogram"
    git config --global status.showUntrackedFiles "all"
    git config --global credential.helper "cache --timeout=86400"
    git config --global core.quotepath "false"
    git config --global core.pager "delta"
    git config --global delta.syntax-theme "OneHalfDark"
    git config --global alias.s "!git status -sb && git stash list"
    git config --global alias.d "diff"
    git config --global alias.ds "diff --stat"
    git config --global alias.dc "diff --cached"
    git config --global alias.dcs "diff --cached --stat"
    git config --global alias.dch "diff --check"
    git config --global alias.l 'log --all --graph --abbrev-commit --pretty=format:"%C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)"'
    git config --global alias.lf 'log -p -M --follow --abbrev-commit --pretty=format:"%C(bold blue)commit%C(reset) %C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)\\n"'
    git config --global alias.lsf 'log -p -M --follow --abbrev-commit --pretty=format:"%C(bold blue)commit%C(reset) %C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)\\n" -S'
    git config --global alias.lgf 'log -p -M --follow --abbrev-commit --pretty=format:"%C(bold blue)commit%C(reset) %C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)\\n" -G'
    git config --global alias.bb "branch -a -vv"
    git config --global alias.bm "branch --merged"
    git config --global alias.bnm "branch --no-merged"
    git config --global alias.ch "checkout"
    git config --global alias.a "!git add -A && git s"
    git config --global alias.cm "!git dch && git commit"
  `
  await removeFile(`${home}/.gitconfig`)
  for (const cmd of cmds.split("\n")) { await exec(cmd) }
  const gitignore = ["*~", "node_modules"]
  await writeFile(`${home}/.gitignore`, gitignore.join("\n"))
}

async function tmux() {
  await copyFile(".tmux.conf", `${home}/.tmux.conf`)
}

async function zsh() {
  await copyFile(".zshrc", `${home}/.zshrc`)
  const path = `${home}/.config`
  await mkdir(path, { recursive: true })
  await copyFile("starship.toml", `${path}/starship.toml`)
}

async function emacs() {
  await mkdir(`${home}/.doom.d`, { recursive: true })
  for (const file of ["packages.el", "init.el", "config.el"]) {
    await copyFile(file, `${home}/.doom.d/${file}`)
  }
}

async function lilypond() {
  const path = `${home}/.doom.d/lilypond`
  await mkdir(path, { recursive: true })
  const url = `${rawGitHub}/lilypond/lilypond/master/elisp`
  const files = [
    "lilypond-font-lock.el", "lilypond-indent.el", "lilypond-init.el",
    "lilypond-mode.el", "lilypond-song.el", "lilypond-what-beat.el"
  ]
  Promise.all(files.map(file => download(`${url}/${file}`, `${path}/${file}`)))
}

async function psql() {
  await copyFile(".psqlrc", `${home}/.psqlrc`)
}

for (const arg of process.argv.slice(2)) {
  switch (arg) {
    case "git": await git(); break
    case "tmux": await tmux(); break
    case "zsh": await zsh(); break
    case "emacs": await emacs(); break
    case "lilypond": await lilypond(); break
    case "psql": await psql(); break
    case "all":
      await Promise.all(
        [git(), tmux(), zsh(), emacs(), lilypond(), psql()]
      ); break
    default: console.log(`WARNING: unknown option ${arg}`)
  }
}
