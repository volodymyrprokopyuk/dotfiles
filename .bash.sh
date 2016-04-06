# 256 color terminal
export TERM=screen-256color
# terminal vim mode
set -o vi
# emacs server mode
alias em='em -nw'
# terminal editor
export EDITOR=em

# colored ls
alias ls='ls --color=auto'
# colored grep
alias grep='grep --color=auto'
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

# install Git
[ -s "$HOME/bin/git-completion.bash" ] \
  && source "$HOME/bin/git-completion.bash"
[ -s "$HOME/bin/git-prompt.sh" ] \
  && source "$HOME/bin/git-prompt.sh" \
  && export GIT_PS1_SHOWDIRTYSTATE=1 \
  && PS1='\[\033[0;37m\]\[\033[0;35m\]\u@\h\[\033[0;37m\]:\[\033[0;34m\]\w\[\033[0;33m\]$(__git_ps1 " (%s)") \[\033[0;36m\]>\[\033[0;00m\] '
# install NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] \
  && source "$NVM_DIR/nvm.sh" && source "$NVM_DIR/bash_completion"
# install ConTeXt
export OSFONTDIR="/usr/local/share/fonts"
[ -s "$HOME/context/tex/setuptex" ] \
  && source ~/context/tex/setuptex

# local software PATH
PATH="$HOME/bin":$PATH
