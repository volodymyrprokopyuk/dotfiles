export PATH=$HOME/.local/bin:$PATH

export TERM=screen-256color
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export LESS="-XFR"
export ANSIBLE_NOCOWS=1

alias tmux="tmux -2"
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
bindkey "^R" history-incremental-pattern-search-backward

alias ee="emacsclient -t"
alias ll="ls -alh --color=auto"
alias gg="ag --hidden --follow --ignore '*~' --ignore .git --ignore .idea --ignore __pycache__ --ignore venv --color-match '1;31'"

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

ex() {
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

jwt() {
    cut -d . -f 1 <<< $1 | base64 -d | jq # JWT Header
    cut -d . -f 2 <<< $1 | base64 -d | jq # JWT Body
    # JWT Signature
}

gll() {
    local log=(git l "$@")
    local preview='p() { set -- $(echo -- "$@" | grep -o "[a-f0-9]\{7\}"); [ $# -eq 0 ] || git d --color=always $1^!; }; p {}'
    local filter=(fzf --ansi --no-sort --preview $preview)
    $log | $filter
}

# Install Liquidprompt
# ZPACKAGE=$HOME/.zsh/liquidprompt/liquidprompt
# [ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE

# Install PowerLevel9k
# POWERLEVEL9K_PROMPT_ON_NEWLINE=true
# POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX="%F{green}┌─%f"
# POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="%F{green}└─➤ %f"
# ZPACKAGE=$HOME/.zsh/powerlevel9k/powerlevel9k.zsh-theme
# [ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE

Install Spaceship Prompt
fpath=($HOME/.zsh/spaceship-prompt $fpath)
ln -sf $HOME/.zsh/spaceship-prompt/spaceship.zsh $HOME/.zsh/spaceship-prompt/prompt_spaceship_setup
autoload -U promptinit; promptinit
prompt spaceship

# Install Pure
# fpath=($HOME/.zsh/pure $fpath)
# ln -sf $HOME/.zsh/pure/pure.zsh $HOME/.zsh/pure/prompt_pure_setup
# ln -sf $HOME/.zsh/pure/async.zsh $HOME/.zsh/pure/async
# autoload -U promptinit; promptinit
# prompt pure


# Install Zsh autosuggestions
ZPACKAGE=$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE
bindkey "^[j" autosuggest-accept

# Install Zsh syntax highlighting
ZPACKAGE=$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE

# Install Zsh completions
fpath=($HOME/.zsh/zsh-completions/src $HOME/.zsh/extra-completions $fpath)
[[ $- = *i* ]] && autoload -Uz compinit && compinit

# Install aws completions
ZPACKAGE=$HOME/.local/bin/aws_zsh_completer.sh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE

# Install fzf
ZPACKAGE=/usr/share/fzf/key-bindings.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE
ZPACKAGE=/usr/share/fzf/completion.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE
export FZF_DEFAULT_COMMAND="ag --nocolor --nogroup --hidden --ignore '*~' --ignore .git --ignore .idea -g ''"
export FZF_DEFAULT_OPTS="--no-height --cycle"
export FZF_CTRL_T_OPTS="--preview '(cat {} || (ls -alh --color=always {} | grep -v '~$')) 2> /dev/null | head -200'"
export FZF_ALT_C_OPTS="--preview 'ls -alh --color=always {} | grep -v '~$' | head -200'"
