# local software PATH
PATH="$HOME/local/bin":$PATH
MANPATH="$HOME/local/share/man":$MANPATH

# 256 color terminal
export TERM=screen-256color
# terminal vim mode
set -o vi
# emacs server mode
alias em='em -nw'
# terminal editor
export EDITOR='em -nw'

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

# install Liquid Prompt
[ -s "$HOME/local/bin/liquidprompt/liquidprompt" ] \
  && [[ $- = *i* ]] && source "$HOME/local/bin/liquidprompt/liquidprompt"
# install Git
[ -s "$HOME/local/bin/git-completion.bash" ] \
  && source "$HOME/local/bin/git-completion.bash"
git config --global user.name 'Volodymyr Prokopyuk'
git config --global user.email 'volodymyrprokopyuk@gmail.com'
git config --global core.editor 'em -nw'
git config --global alias.lg "log --all --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias tig='tig --all'
# install NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] \
  && source "$NVM_DIR/nvm.sh" && source "$NVM_DIR/bash_completion"
# install ConTeXt
export OSFONTDIR="/usr/local/share/fonts"
[ -s "$HOME/context/tex/setuptex" ] \
  && source ~/context/tex/setuptex
