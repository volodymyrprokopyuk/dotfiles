# Dotfiles

Config files for:

1. [Bash](http://www.gnu.org/software/bash/)
2. [Tmux](http://tmux.github.io/)
3. [Emacs](http://www.gnu.org/software/emacs/)
4. [Vim](http://www.vim.org/)
5. [psql](http://www.postgresql.org/docs/9.4/static/app-psql.html)

# Install

1. [Shake](http://shakebuild.com/)
```bash
$ sudo apt-get install ghc haskell-platform
$ cabal update
$ cabal install shake
```
2. Install config files
```bash
$ ./build
```
3. Attach Bash config
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
4. Start Tmux
In `~/.bashrc`:
```bash
# start Tmux
tmux attach-session -t "$USER" || tmux new-session -s "$USER"
```
5. Attach Emacs config
In `~/.emacs`:
```elisp
; Emacs config
(load "~/.emacs.d/emacs.el")
```
6. Install em
```bash
$ sudo ln -s ~/.emacs.d/em /usr/local/bin
```
