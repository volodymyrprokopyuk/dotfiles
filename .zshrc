# local software path
export PATH=$HOME/local/bin:$PATH
export MANPATH=$HOME/local/share/man:$MANPATH
export INFOPATH=$HOME/local/share/info:$INFOPATH
# 256 color terminal
export TERM=screen-256color
# terminal editor
alias em='em -nw'
export EDITOR='em -nw'

# terminal vim mode
bindkey -v
# history
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
bindkey "^R" history-incremental-search-backward
# completion
autoload -Uz compinit && compinit

setopt appendhistory autocd

# start tmux
alias tmux="tmux -2"
[[ -z "$TMUX" ]] && (tmux attach-session -t $USER || tmux new-session -s $USER)

# Bash aliases
alias ..='cd ..'
alias md='mkdir -p'
# colored ls
alias ls='ls --color=auto'
alias l='ls -lAh'
# colored grep
alias grep='grep --color=auto'
# colored less
alias less='less -r'
# vifm file manager
alias fm='vifm'
# ag
alias ag='ag --hidden --ignore *~ --ignore .git'
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

# install Zsh history substring search
[ -s $HOME/local/bin/zsh-history-substring-search/zsh-history-substring-search.zsh ] \
  && [[ $- = *i* ]] && source $HOME/local/bin/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# install Liquid Prompt
[ -s $HOME/local/bin/liquidprompt/liquidprompt ] \
  && [[ $- = *i* ]] && source $HOME/local/bin/liquidprompt/liquidprompt
# install Git
#[ -s $HOME/local/bin/git-completion.bash ] \
#  && source $HOME/local/bin/git-completion.bash
alias tig='tig --all'
# install Ninja
#[ -s $HOME/local/bin/ninja-completion.bash ] \
#  && source $HOME/local/bin/ninja-completion.bash
alias ninja='ninja -v'
# install Kerl
[ -s $HOME/local/erlang/activate ] \
  && source $HOME/local/erlang/activate
#[ -s $HOME/local/bin/kerl-completion.bash ] \
#  && source $HOME/local/bin/kerl-completion.bash
# install Elixir
[ -s $HOME/local/elixir/bin/elixir ] && PATH=$HOME/local/elixir/bin:$PATH
alias iexm='iex -S mix'
# install NVM
export NVM_DIR=$HOME/.nvm
[ -s $NVM_DIR/nvm.sh ] && source $NVM_DIR/nvm.sh
[ -s $NVM_DIR/bash_completion ] && source $NVM_DIR/bash_completion
# install ConTeXt
#export OSFONTDIR=/usr/local/share/fonts
#[ -s $HOME/context/tex/setuptex ] && source $HOME/context/tex/setuptex
# install Gradle
export GRADLE_HOME=$HOME/local/gradle
[ -s $HOME/local/gradle/bin/gradle ] && PATH=$HOME/local/gradle/bin:$PATH
