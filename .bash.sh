#
# bash configuration
#
# author: Volodymyr Prokopyuk
#
# in ~/.bashrc:
#   # volodymyr's configuration
#   source ~/.bash.sh
#   # start tmux
#   tmux attach-session -t "$USER" || tmux new-session -s "$USER"
#   # emacs client (em)
#   $ sudo ln -s ~/.emacs.d/em /usr/local/bin
#
# in ~/.bash_profile:
#   # volodymyr's configuration
#   source ~/.bash.sh
#
# 256 color mode
export TERM=screen-256color
# editor
export EDITOR=vim
# vim mode
set -o vi
# emacs terminal server mode
alias em='em -nw'

# colored ls
alias ls='ls --color=auto'
# colored egrep
alias egrep='egrep --color'
# colored less
alias less='less -r'
# mc color scheme
alias mc='mc -S xoria256'
# colored man
man() {
  env \
  LESS_TERMCAP_mb=$'\e[01;31m' \
  LESS_TERMCAP_md=$'\e[01;38;5;74m' \
  LESS_TERMCAP_me=$'\e[0m' \
  LESS_TERMCAP_se=$'\e[0m' \
  LESS_TERMCAP_so=$'\e[38;5;246m' \
  LESS_TERMCAP_ue=$'\e[0m' \
  LESS_TERMCAP_us=$'\e[04;38;5;146m' \
  man "$@"
}

# source NVM
# $ nvm alias default v0.12.x
source ~/.nvm/nvm.sh
source ~/.nvm/bash_completion

# install ConTeXt
#  $ mkdir ~/context && cd ~/context
#  $ wget http://minimals.contextgarden.net/setup/first-setup.sh
#  http://http://minimals.contextgarden.net/current/context
#  $ sh ./first-setup.sh --context=current --engine=luatex --modules=all
# source ConTeXt
export OSFONTDIR=/usr/local/share/fonts
source ~/context/tex/setuptex
# update ConTeXt
#  $ cd ~/context
#  $ rsync -ptv rsync://contextgarden.net/minimals/setup/first-setup.sh .
#  http://http://minimals.contextgarden.net/current/context
#  $ sh ./first-setup.sh --context=current --engine=luatex --modules=all
# remake format
#  $ mtxrun --generate
#  $ context --make

# add Cabal to PATH
export PATH=~/.cabal/bin:$PATH
# add Julia to PATH
export PATH=~/julia/usr/bin:$PATH
