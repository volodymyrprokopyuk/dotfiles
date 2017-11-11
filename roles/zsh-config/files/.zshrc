export PATH=$HOME/local/bin:$PATH

export TERM=screen-256color
export ALTERNATE_EDITOR=''
export EDITOR='emacsclient -t'
export ANSIBLE_NOCOWS=1

alias tmux='tmux -2'
[[ $- = *i* ]] && [[ -z $TMUX ]] && (tmux attach-session -t $USER || tmux new-session -s $USER)

setopt AUTO_CD
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt EXTENDED_HISTORY
setopt EXTENDED_GLOB

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
bindkey '^R' history-incremental-pattern-search-backward

alias ee='emacsclient -t'
alias ll='ls -A -l -h --color=auto'
alias ss="ag --hidden --ignore *~ --ignore .git --ignore .idea --color-match '1;31'"

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

# Install Liquidprompt
#ZPACKAGE=$HOME/.zsh/liquidprompt/liquidprompt
#[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE

# Install PowerLevel9k
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX="%F{green}┌─%f"
POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="%F{green}└─➤ %f"
#POWERLEVEL9K_DIR_BOLD=true
ZPACKAGE=$HOME/.zsh/powerlevel9k/powerlevel9k.zsh-theme
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE

# Install Zsh autosuggestions
ZPACKAGE=$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE
bindkey '^[;' autosuggest-accept

# Install Zsh syntax highlighting
ZPACKAGE=$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE

# Install Zsh completions
fpath=($HOME/.zsh/zsh-completions/src $HOME/.zsh/extra-completions $fpath)
[[ $- = *i* ]] && autoload -Uz compinit && compinit

# Install fzf
ZPACKAGE=$HOME/.fzf.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE
export FZF_DEFAULT_COMMAND='ag --nocolor --nogroup --hidden --ignore *~ --ignore .git --ignore .idea -g ""'
export FZF_DEFAULT_OPTS="--cycle"

# Install Java
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64

# Install Maven
export M2_HOME=$HOME/local/maven
[ -s $HOME/local/maven/bin/mvn ] && PATH=$HOME/local/maven/bin:$PATH

# Install Gradle
export GRADLE_HOME=$HOME/local/gradle
[ -s $HOME/local/gradle/bin/gradle ] && PATH=$HOME/local/gradle/bin:$PATH

# Install NVM
export NVM_DIR=$HOME/.nvm
ZPACKAGE=$NVM_DIR/nvm.sh
[ -s $ZPACKAGE ] && source $ZPACKAGE
