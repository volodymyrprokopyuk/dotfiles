# local software PATH
PATH=$HOME/local/bin:$PATH
MANPATH=$HOME/local/share/man:$MANPATH

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
alias ll='ls -lah'
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
  man $@
}

# Bash aliases
alias l='ls -lah'
alias mf='touch'
alias md='mkdir'

# install Liquid Prompt
[ -s $HOME/local/bin/liquidprompt/liquidprompt ] \
  && [[ $- = *i* ]] && source $HOME/local/bin/liquidprompt/liquidprompt
# install Git
[ -s $HOME/local/bin/git-completion.bash ] \
  && source $HOME/local/bin/git-completion.bash
alias tig='tig --all'
# install Ninja
[ -s $HOME/local/bin/ninja-completion.bash ] \
  && source $HOME/local/bin/ninja-completion.bash
# install Kerl
[ -s $HOME/local/erlang/activate ] \
  && source $HOME/local/erlang/activate
[ -s $HOME/local/bin/kerl-completion.bash ] \
  && source $HOME/local/bin/kerl-completion.bash
# install Elixir
[ -s $HOME/local/elixir/bin/elixir ] && PATH=$HOME/local/elixir/bin:$PATH
alias iexm='iex -S mix'
# install NVM
export NVM_DIR=$HOME/.nvm
[ -s $NVM_DIR/nvm.sh ] && source $NVM_DIR/nvm.sh
[ -s $NVM_DIR/bash_completion ] && source $NVM_DIR/bash_completion
# install ConTeXt
export OSFONTDIR=/usr/local/share/fonts
[ -s $HOME/context/tex/setuptex ] && source $HOME/context/tex/setuptex
# install Gradle
export GRADLE_HOME=$HOME/local/gradle
[ -s $HOME/local/gradle/bin/gradle ] && PATH=$HOME/local/gradle/bin:$PATH
