export PATH=$HOME/local/bin:$PATH
export MANPATH=$HOME/local/share/man:$MANPATH
export INFOPATH=$HOME/local/share/info:$INFOPATH

export TERM=screen-256color
export EDITOR='em -nw'
bindkey -v

alias tmux='tmux -2'
[[ $- = *i* ]] && [[ -z $TMUX ]] \
  && (tmux attach-session -t $USER || tmux new-session -s $USER)

setopt AUTO_CD
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt EXTENDED_HISTORY

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
bindkey '^R' history-incremental-pattern-search-backward

alias e='em -nw'
alias l='ls -A -l -h --color=auto'
alias _ag="ag --hidden --ignore *~ --ignore .git --color-match '1;31'"
alias s='_ag'
alias sf='_ag -g'
alias g='git'

alias less='less -r'
alias info='info --vi-keys'
alias grep='grep -r -n --color=auto'
alias fm='vifm'
alias tig='tig --all'
alias ninja='ninja -v'

function man {
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

function unpack {
  echo Unpacking $1 ...
  if [ -f $1 ] ; then
    case $1 in
      (*.tar.gz|*.tgz) tar xzf $1 ;;
      (*.tar.bz2|*.tbz2) tar xjf $1 ;;
      (*.tar.xz|*.txz) tar xJf $1 ;;
      (*.tar) tar xf $1 ;;
      (*.gz) gunzip $1 ;;
      (*.bz2) bunzip2 $1 ;;
      (*.zip) unzip $1 ;;
      (*.rar) unrar x $1 ;;
      (*.Z) uncompress $1 ;;
      (*.7z) 7z x $1 ;;
      (*) echo "'$1' unknown archive format" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

function apt-upgrade {
  sudo sh -c 'apt-get update && apt-get dist-upgrade --yes && apt-get autoremove && apt-get autoclean'
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

# add java to path
export JAVA_HOME=$HOME/local/java
[ -s $HOME/local/java/bin/java ] && PATH=$HOME/local/java/bin:$PATH

# add gradle to path
export GRADLE_HOME=$HOME/local/gradle
[ -s $HOME/local/gradle/bin/gradle ] && PATH=$HOME/local/gradle/bin:$PATH

# add maven to path
export M2_HOME=$HOME/local/maven
[ -s $HOME/local/maven/bin/mvn ] && PATH=$HOME/local/maven/bin:$PATH

# install nvm
export NVM_DIR=$HOME/.nvm
ZPACKAGE=$NVM_DIR/nvm.sh
[ -s $ZPACKAGE ] && source $ZPACKAGE

# install kerl
ZPACKAGE=$HOME/local/erlang/activate
[ -s $ZPACKAGE ] && source $ZPACKAGE

# add elixir to path
[ -s $HOME/local/elixir/bin/elixir ] && PATH=$HOME/local/elixir/bin:$PATH
alias iexm='iex -S mix'
