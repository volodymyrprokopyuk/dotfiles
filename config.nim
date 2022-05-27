import std/[strutils, parseopt, os, osproc]

type
  OptionError = object of ValueError
  ShellError = object of OSError

let home = getHomeDir()

template shell(cmd: string) =
  let exitCode = execCmd(cmd)
  if exitCode != 0:
    raise newException(ShellError, "non-zero exit code: " & $exitCode)

proc configGit() =
  let cmds = """
    git config --global user.name "Volodymyr Prokopyuk"
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
    git config --global alias.lf 'log -p -M --follow --abbrev-commit --pretty=format:"%C(bold blue)commit%C(reset) %C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)\n"'
    git config --global alias.lsf 'log -p -M --follow --abbrev-commit --pretty=format:"%C(bold blue)commit%C(reset) %C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)\n" -S'
    git config --global alias.lgf 'log -p -M --follow --abbrev-commit --pretty=format:"%C(bold blue)commit%C(reset) %C(red)%h%C(reset) -%C(bold yellow)%d%C(reset) %C(yellow)%s%C(reset) %C(green)<%an>%C(reset) %C(blue)(%cr)%C(reset)\n" -G'
    git config --global alias.bb "branch -a -vv"
    git config --global alias.bm "branch --merged"
    git config --global alias.bnm "branch --no-merged"
    git config --global alias.ch "checkout"
    git config --global alias.a "!git add -A && git s"
    git config --global alias.cm "!git dch && git commit"
  """
  removeFile(home / ".gitconfig")
  for cmd in cmds.unindent.split('\n'): shell(cmd)
  let gitignore = ["*~", "node_modules", ""]
  writeFile(home / ".gitignore", gitignore.join("\n"))

proc configTmux() =
  copyFile(".tmux.conf", home / ".tmux.conf")

proc configZsh() =
  copyFile(".zshrc", home / ".zshrc")

proc configEmacs() =
  createDir(home / ".doom.d")
  for file in ["packages.el", "init.el", "config.el"]:
    copyFile(file, home / ".doom.d" / file)

proc configPsql() =
  copyFile ".psqlrc", home / ".psqlrc"

proc configureTools() =
  for kind, key, value in getopt():
    if kind == cmdShortOption and key == "c":
      case value:
      of "git": configGit()
      of "tmux": configTmux()
      of "zsh": configZsh()
      of "emacs": configEmacs()
      of "psql": configPsql()
      of "all":
        configGit()
        configTmux()
        configZsh()
        configEmacs()
        configPsql()
      else: raise newException(OptionError, "unknown tool: " & value)
    else: raise newException(OptionError, "unknown option: " & key)

configureTools()
