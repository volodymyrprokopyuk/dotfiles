export TERM=screen-256color
export EDITOR='emacsclient -t'
export ALTERNATE_EDITOR='emacsclient -t'
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

# Command line editing
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# Tools
function ll {
  exa --all --long --sort=type --color-scale \
    --git --ignore-glob='*~|.git|node_modules' $@
}
function lll { ll -T $@ }
function vv {
  bat --style plain --theme 1337 --tabs 2 \
    --map-syntax '*.conf:INI' --map-syntax '*.lys:TeX' $@
}
function ff { fd --follow --hidden --exclude .git $@ }
function gg { rg --hidden --follow $@ }
function nnn_config {
  BLK="8B" CHR="D2" DIR="33" EXE="C5" REG="FC"
  HLNK="BF" SLNK="E3" MISS="7C" ORPH="80"
  FIFO="1A" SOCK="2F" OTH="E7"
  export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HLNK$SLNK$MISS$ORPH$FIFO$SOCK$OTH"
}
nnn_config
function nn { nnn -A $@ }
function ee { emacsclient -t $@ }

# Man
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

# Starship prompt
eval "$(starship init zsh)"

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
# C-r search command history
# C-t complete current command
# A-c change directory
script_source /usr/share/fzf/key-bindings.zsh
script_source /usr/share/fzf/completion.zsh
export FZF_DEFAULT_COMMAND='fd --follow --hidden --exclude .git --exclude node_modules --no-ignore --color always'
export FZF_DEFAULT_OPTS="--ansi --no-height --cycle --bind alt-j:down,alt-k:up"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# C-o opne file with xdg-open
function fzf-open-file {
  local file
  { file="$(fzf)" && [ -f "$file" ] && xdg-open "$file" &> /dev/null } </dev/tty
}
zle -N fzf-open-file
bindkey '^o' fzf-open-file

# Doom Emacs
path_add $HOME/.emacs.d/bin

# Node.js
[ -z $NVM_DIR ] && export NVM_DIR=$HOME/.nvm
script_source /usr/share/nvm/nvm.sh
script_source /usr/share/nvm/install-nvm-exec

# Lilypond
path_add $HOME/.lilypond/bin

# Nim
path_add $HOME/.nimble/bin
