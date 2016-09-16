alias tmux='tmux -2'
[[ $- = *i* ]] && [[ -z $TMUX ]] \
  && (tmux attach-session -t $USER || tmux new-session -s $USER)

setopt AUTO_CD
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt EXTENDED_HISTORY

export TERM=screen-256color
export EDITOR='em -nw'
bindkey -v

export PATH=$HOME/local/bin:$PATH
export MANPATH=$HOME/local/share/man:$MANPATH
export INFOPATH=$HOME/local/share/info:$INFOPATH

export HISTFILE=~/.histfile
export HISTSIZE=10000
export SAVEHIST=10000
bindkey '^R' history-incremental-pattern-search-backward

alias em='em -nw'
alias fm='vifm'
alias tig='tig --all'
alias ninja='ninja -v'
alias ls='ls --color=auto'
alias l='ls -lAh'
alias grep='grep --color=auto'
alias ag='ag --hidden --ignore *~ --ignore .git'
alias less='less -r'
alias info='info --vi-keys'

function man() {
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

# install liquid prompt
ZPACKAGE=$HOME/.zsh/liquidprompt/liquidprompt
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE

# install zsh autosuggestions
ZPACKAGE=$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE
bindkey '^[;' autosuggest-accept

# install zsh syntax highlighting
ZPACKAGE=$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE

# install zsh completions
fpath=($HOME/.zsh/zsh-completions/src $HOME/.zsh/completions $fpath)
[[ $- = *i* ]] && autoload -Uz compinit && compinit

# install nvm
export NVM_DIR=$HOME/.nvm
ZPACKAGE=$NVM_DIR/nvm.sh
[ -s $ZPACKAGE ] && source $ZPACKAGE

# add gradle to path
export GRADLE_HOME=$HOME/local/gradle
[ -s $HOME/local/gradle/bin/gradle ] && PATH=$HOME/local/gradle/bin:$PATH

# install kerl
ZPACKAGE=$HOME/local/erlang/activate
[ -s $ZPACKAGE ] && source $ZPACKAGE

# add elixir to path
[ -s $HOME/local/elixir/bin/elixir ] && PATH=$HOME/local/elixir/bin:$PATH
alias iexm='iex -S mix'

# install context
#export OSFONTDIR=/usr/local/share/fonts
#ZPACKAGE=$HOME/context/tex/setuptex
#[ -s $ZPACKAGE ] && source $ZPACKAGE
