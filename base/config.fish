set -gx PATH $PATH $HOME/.config/{emacs,lilypond}/bin
set -gx EDITOR emacs -nw
set -gx PAGER less
set -gx LESS -RF
set -gx LS_COLORS (vivid generate snazzy)
set -gx BAT_STYLE plain
set -gx BAT_THEME 1337

set -g fish_greeting

set -g fish_color_normal D7E8FE
set -g fish_color_error FF6C6C
set -g fish_color_cancel FF7F56
set -g fish_color_autosuggestion BABAFF

set -g fish_color_command FFF467 --bold
set -g fish_color_option FFAF3B
set -g fish_color_param FFED95

set -g fish_color_keyword FFFFFF --bold
set -g fish_color_quote FF8C62
set -g fish_color_valid_path 61FDA5 --underline

set -g fish_color_operator 7DCEFA
set -g fish_color_redirection 79FB6B
set -g fish_color_end 69F7F7

set -g fish_color_comment 00B567 --italics
set -g fish_color_escape 23A700

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
