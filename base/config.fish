set -gx PATH $PATH $HOME/.config/{emacs,lilypond}/bin
set -gx EDITOR emacs -nw
set -gx PAGER less
set -gx LESS -RF
set -gx LS_COLORS (vivid generate snazzy)
set -gx BAT_STYLE plain
set -gx BAT_THEME 1337
set -U fish_greeting
bind \em accept-autosuggestion

starship init fish | source

function lla
  eza --all --sort=type --long --git \
    --time-style=relative --smart-group --no-permissions --octal-permissions \
    --color-scale --no-quotes $argv
end

function ll
  lla --git-ignore $argv
end

function lll
  ll --tree $argv
end

function ff
  fd --hidden $argv
end

function gg
  rg --hidden --glob=!.git $argv
end

function vv
  bat --tabs=2 $argv
end

function ee
  emacs -nw $argv
end
