export PATH=$HOME/.local/bin:$PATH

export TERM=screen-256color
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export LESS="-XFR"

alias tmux="tmux -2"
[[ $- = *i* ]] && [[ -z $TMUX ]] && (tmux attach-session -t $USER || tmux new-session -s $USER)

setopt AUTO_CD
setopt EXTENDED_GLOB

# Configure history
# Ctrl-r (search history)
# Ctrl-p/Ctrl-n (previous/next command)
# Ctrl-g (discard)
readonly HISTFILE=~/.histfile
readonly HISTSIZE=10000
readonly SAVEHIST=10000
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS
bindkey "^R" history-incremental-pattern-search-backward

# Command line editing
# Ctrl-a/Ctrl-e (beginning/end of line)
# Alt-f/Alt-b (forward/backward word)
# Ctrl-w (delete word backword)
# Ctrl-x, e (edit command line)
# Ctrl-l (clear screen)
autoload -U edit-command-line
zle -N edit-command-line
bindkey "^xe" edit-command-line
bindkey "^x^e" edit-command-line

# Alias
alias ll="exa --all --long --sort=type --git --git-ignore --ignore-glob='*~|.git|node_modules|coverage|pyvenv|__pycache__|.pytest_cache|htmlcov'"
alias vv="bat --style plain --theme zenburn --tabs 4 --map-syntax conf:ini"
alias gg="ag --hidden --follow --color-match '1;31'"
alias ee="emacsclient -t"
alias uu="curl -sSLk"

function pp {
    ag --nocolor --nogroup --hidden --follow -g '' $@ \
    | fzf \
    --bind \
    ctrl-j:preview-down,ctrl-k:preview-up,ctrl-f:preview-page-down,ctrl-b:preview-page-up \
    --preview \
    'bat --color always --style plain --theme zenburn --tabs 4 --map-syntax conf:ini {}'
}

# curl options
# -s silent -S show error -L follow redirects -k insecure
# -X request -H header -u user:password -d 'data', @file
# -D - dump response header

# Regular expressions
# - (?:...) non-capturing group
# - (?=...) positive lookahead
# - (?!...) negative lookahead
# - (?<=...) positive lookbehind
# - (?<!...) negative lookbehind

# Syntax highlight man page
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

# Extract archive
function ex {
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

# Merge multiple PDF files into a single PDF file
# Usage: pdfmerge merged.pdf source1.pdf source2.pdf
function pdfmerge {
    gs -dNOPAUSE -dBATCH -dQUIET -dPDFSETTINGS=/prepress -sDEVICE=pdfwrite \
        -sOutputFile=$@
    # convert -density 200 -quality 60 -compress jpeg source.pdf converted.pdf
}

# Install Spaceship Prompt
fpath=($HOME/.zsh/spaceship-prompt $fpath)
ln -sf $HOME/.zsh/spaceship-prompt/spaceship.zsh $HOME/.zsh/spaceship-prompt/prompt_spaceship_setup
autoload -U promptinit; promptinit
prompt spaceship
readonly SPACESHIP_CHAR_SYMBOL='▶ '
readonly SPACESHIP_EXIT_CODE_SHOW=true
readonly SPACESHIP_EXIT_CODE_SYMBOL='● '

# Install Zsh syntax highlighting
ZPACKAGE=$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE

# Install Zsh completions
fpath=($HOME/.zsh/zsh-completions/src $HOME/.zsh/extra-completions $fpath)
[[ $- = *i* ]] && autoload -Uz compinit && compinit

# Install AWS completions
ZPACKAGE=/usr/bin/aws_zsh_completer.sh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE

# Install GCP completions
ZPACKAGE=/opt/google-cloud-sdk/path.zsh.inc
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE
ZPACKAGE=/opt/google-cloud-sdk/completion.zsh.inc
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE

# Install fzf
ZPACKAGE=/usr/share/fzf/key-bindings.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE
ZPACKAGE=/usr/share/fzf/completion.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE
export FZF_DEFAULT_COMMAND="ag --nocolor --nogroup --hidden --follow -g ''"
export FZF_DEFAULT_OPTS="--no-height --cycle --bind alt-j:down,alt-k:up"
export FZF_COMPLETION_TRIGGER=''
bindkey '^T' fzf-completion
bindkey '^I' $fzf_default_completion

# Install Zsh autosuggestions
ZPACKAGE=$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
[ -s $ZPACKAGE ] && [[ $- = *i* ]] && source $ZPACKAGE
bindkey -r "^[,"
bindkey "^[," autosuggest-accept

# Install NVM
export NVM_DIR=~/.nvm
readonly NVM_SOURCE=/usr/share/nvm
[[ -s $NVM_SOURCE/nvm.sh ]] && source $NVM_SOURCE/nvm.sh

# Install Yarn
type yarn > /dev/null 2>&1 && export PATH=$(yarn global bin):$PATH || true
