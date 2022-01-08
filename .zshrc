export TERM=screen-256color
export EDITOR='emacsclient -t'
export PAGER=less
export LESS='-RF'

function path_add { [[ -d $1 ]] && export PATH=$1:$PATH }
function script_source { [[ -s $1 ]] && [[ $- = *i* ]] && source $1 }

# ls colors
script_source /usr/share/LS_COLORS/dircolors.sh

# Tmux
[[ $- = *i* ]] && [[ -z $TMUX ]] && \
  (tmux -2 attach-session -t $USER || tmux -2 new-session -s $USER)

# Zsh history
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
bindkey '^R' history-incremental-pattern-search-backward

# Command line editing
# Ctrl-a/Ctrl-e (beginning/end of line)
# Alt-f/Alt-b (forward/backward word)
# Ctrl-w (delete word backword)
# Ctrl-x, e (edit command line)
# Ctrl-l (clear screen)
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# Alias
function ll { exa --all --long --sort=type --color-scale --git --ignore-glob='*~|.git' $@ }
function vv { bat --style plain --theme zenburn --tabs 4 --map-syntax '*.conf:INI' $@ }
function ff { fd --follow --hidden --exclude .git $@ }
function gg { ag --hidden --follow --color-match '1;31' $@ }
function ee { emacsclient -t $@ }
function pp {
  # ag --nocolor --nogroup --hidden --follow -g '' $@ \
  fd --type file --follow --hidden --exclude .git --color always $@ \
  | fzf \
  --bind \
  ctrl-j:preview-down,ctrl-k:preview-up,ctrl-f:preview-page-down,ctrl-b:preview-page-up \
  --preview \
  'bat --color always --style plain --theme zenburn --tabs 4 --map-syntax conf:INI {}'
}

# man
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

# Zinit
script_source $HOME/.local/share/zinit/zinit.git/zinit.zsh

# Spaceship prompt
zinit light denysdovhan/spaceship-prompt.git
readonly SPACESHIP_CHAR_SYMBOL='▶ '
readonly SPACESHIP_EXIT_CODE_SHOW=true
readonly SPACESHIP_EXIT_CODE_SYMBOL='● '

# Zsh fast syntax highlighting
zinit light zdharma-continuum/fast-syntax-highlighting.git

# Zsh completions
zinit light zsh-users/zsh-completions.git
autoload -U compinit && compinit

# Zsh autosuggestions
zinit light zsh-users/zsh-autosuggestions.git
bindkey -r '^[,'
bindkey '^[,' autosuggest-accept

# Fzf
script_source /usr/share/fzf/key-bindings.zsh
script_source /usr/share/fzf/completion.zsh
export FZF_DEFAULT_OPTS="--ansi --no-height --cycle --bind alt-j:down,alt-k:up"

# Doom Emacs
path_add $HOME/.emacs.d/bin

# Nim
path_add $HOME/.nimble/bin
