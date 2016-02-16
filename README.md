# Dotfiles

Config files for:

1. [Bash](http://www.gnu.org/software/bash/)
2. [Tmux](http://tmux.github.io/)
3. [Emacs](http://www.gnu.org/software/emacs/)
4. [Vim](http://www.vim.org/)
5. [psql](http://www.postgresql.org/docs/9.4/static/app-psql.html)
6. [gnuplot](http://www.gnuplot.info/)
7. [Vifm](http://vifm.info/)

# Install

#### Install [Ninja](https://martine.github.io/ninja/)

```bash
$ sudo apt-get install ninja-build
```

#### Install config files

```bash
$ git clone git@github.com:volodymyrprokopyuk/dotfiles.git
$ cd dotfiles
$ ninja
$ ninja update
```

#### Attach Bash config

In `~/.bashrc`:
```bash
# Bash config
source ~/.bash.sh
```

In `~/.bash_profile`:
```bash
# Bash config
source ~/.bash.sh
```

#### Start Tmux

In `~/.bashrc`:
```bash
# start Tmux
alias tmux="tmux -2"
tmux attach-session -t "$USER" || tmux new-session -s "$USER"
```

#### Attach Emacs config

In `~/.emacs`:
```elisp
; Emacs config
(load "~/.emacs.d/emacs.el")
```

#### Install em

```bash
$ sudo apt-get install xsel
$ sudo ln -s ~/.emacs.d/em /usr/local/bin
```
