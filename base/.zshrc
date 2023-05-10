export TERM=screen-256color
export EDITOR='emacsclient -t'
export ALTERNATE_EDITOR='emacsclient -t'
export PAGER=less
export LESS='-RF'
which vivid &> /dev/null && export LS_COLORS=$(vivid generate snazzy)

readonly HISTFILE=$HOME/.config/zsh/.histfile
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

function path_add { [[ -d $1 ]] && export PATH=$1:$PATH }
function script_source { [[ -s $1 ]] && source $1 }

function ll {
  exa --all --long --sort=type --color-scale \
    --git --ignore-glob='*~|.git|node_modules' $@
}
function lll { ll --tree $@ }
function vv { bat $@ }
function ff {
  fd --hidden --follow --no-ignore \
  --exclude .git --exclude node_modules $@
}
function gg { rg --hidden --follow $@ }
function ee { emacsclient -t $@ }

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

function fzf_config {
  script_source /usr/share/fzf/key-bindings.zsh
  script_source /usr/share/fzf/completion.zsh
  FZF_PREVIEW="bat --color always {} || exa --all --sort=type --tree --level 3 --color-scale {}"
  FZF_FIND="fd --hidden --follow --no-ignore --exclude .git --exclude node_modules --color always"
  export FZF_DEFAULT_COMMAND="$FZF_FIND"
  export FZF_DEFAULT_OPTS="--ansi --no-height --cycle --bind alt-j:down,alt-k:up"
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_CTRL_T_OPTS="--preview '($FZF_PREVIEW) 2> /dev/null'"
  export FZF_ALT_C_COMMAND="$FZF_FIND --type d"
}
fzf_config

# C-o open file with xdg-open
function fzf_open_file {
  local file
  { file="$(fzf)" && [ -f "$file" ] && xdg-open "$file" &> /dev/null } </dev/tty
}
zle -N fzf_open_file
bindkey '^o' fzf_open_file

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

# Doom Emacs
path_add $HOME/.config/emacs/bin

# Node.js
[ -z $NVM_DIR ] && export NVM_DIR=$HOME/.nvm
script_source /usr/share/nvm/nvm.sh
script_source /usr/share/nvm/install-nvm-exec

# LilyPond
path_add $HOME/.lilypond/bin
