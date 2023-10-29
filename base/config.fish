set -gx PATH $PATH $HOME/.config/{emacs,lilypond}/bin
set -gx EDITOR emacs -nw
set -gx PAGER less
set -gx LESS -RF
set -gx LS_COLORS (vivid generate snazzy)
set -gx BAT_STYLE plain
set -gx BAT_THEME 1337

set -g fish_greeting

set -g fish_color_normal FFFFCC # cream yellow
set -g fish_color_autosuggestion 708238 # olive green
set -g fish_color_comment B2AC88 # sage green
set -g fish_color_error FF2400 # scarlet red
set -g fish_color_cancel E52B50 # amaranth red

set -g fish_color_command 98FB98 --bold # mint green
set -g fish_color_option 4CBB17 # kelly green
set -g fish_color_param 9ACD32 # yellow green

set -g fish_color_keyword FDFD96 --bold # pastel yellow
set -g fish_color_quote FEDC56 # mustard yellow
set -g fish_color_escape E4D00A # citrine yellow
set -g fish_color_valid_path DFFF00 --underline # chartreuse yellow

set -g fish_color_operator 00CCFF # vivid sky blue
set -g fish_color_redirection 0ABAB5 # tiffany blue
set -g fish_color_end 007fff # azure blue

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
