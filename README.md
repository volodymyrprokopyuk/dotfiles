# Dotfiles

Config files for:

1. [bash](http://www.gnu.org/software/bash/)
1. [tmux](http://tmux.github.io/)
1. [emacs](http://www.gnu.org/software/emacs/)
1. [vim](http://www.vim.org/)
1. [vifm](http://vifm.info/)
1. [mc](https://www.midnight-commander.org/)
1. [tig](https://github.com/jonas/tig)
1. [lynx](http://lynx.invisible-island.net/lynx.html)
1. [psql](http://www.postgresql.org/docs/9.4/static/app-psql.html)

# Install

#### Install [ninja](https://martine.github.io/ninja/) and utils

```bash
$ sudo apt-get install ninja-build
$ sudo apt-get install xsel
$ sudo apt-get install libxml2-utils
$ sudo apt-get install pandoc
$ sudo apt-get install python-pygments
$ sudo apt-get install htop
$ sudo apt-get install curl
$ sudo apt-get install mercurial
```

#### Install apps

```bash
$ sudo apt-get install vim vim-gtk
$ sudo apt-get install mc
$ sudo apt-get install lynx-cur
```
From source:

- [tmux](https://tmux.github.io/)
- [emacs](https://www.gnu.org/software/emacs/)
- [vifm](http://vifm.info/)
- [git](https://git-scm.com/)
- [tig](https://github.com/jonas/tig)

#### Install config files

```bash
$ git clone git@github.com:volodymyrprokopyuk/dotfiles.git
$ cd dotfiles
$ ninja
$ ninja update
```

#### Attach bash config

In `~/.bashrc`:
```bash
# Bash config
source ~/.bash.sh
```

#### Start tmux

In `~/.bashrc`:
```bash
# start Tmux
alias tmux="tmux -2"
tmux attach-session -t "$USER" || tmux new-session -s "$USER"
```

#### Attach emacs config

In `~/.emacs`:
```elisp
; Emacs config
(load "~/.emacs.d/emacs")
```
