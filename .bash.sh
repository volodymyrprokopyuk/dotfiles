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
# vifm file manager
alias fm='vifm'
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

# install NVM
source ~/.nvm/nvm.sh
source ~/.nvm/bash_completion
# install OCaml
source ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
# install ConTeXt
export OSFONTDIR=/usr/local/share/fonts
source ~/context/tex/setuptex
# add Cabal to PATH
export PATH=~/.cabal/bin:$PATH
