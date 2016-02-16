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
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] \
  && source "$NVM_DIR/nvm.sh" && source "$NVM_DIR/bash_completion"
# install ConTeXt
export OSFONTDIR="/usr/local/share/fonts"
[ -s "$HOME/context/tex/setuptex" ] && source ~/context/tex/setuptex
