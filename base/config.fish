set -gx PATH $HOME/.config/{go,emacs,lilypond,foundry}/bin $PATH
set -gx GOPATH $HOME/.local/go
set -gx EDITOR emacs -nw
set -gx PAGER less
set -gx LESS '-RFQ --no-vbell'
set -gx LS_COLORS (vivid generate snazzy)

set -g fish_greeting
set -g fish_color_normal FFFFCC # cream yellow
set -g fish_color_autosuggestion 708238 # olive green
set -g fish_color_comment B2AC88 # sage green
set -g fish_color_error FF2400 # scarlet red
set -g fish_color_cancel E52B50 # amaranth red
set -g fish_color_command 98FB98 --bold # mint green
set -g fish_color_option FF7F50 # coral red
set -g fish_color_param B2FFFF # celeste blue
set -g fish_color_keyword FDFD96 --bold # pastel yellow
set -g fish_color_quote FEDC56 # mustard yellow
set -g fish_color_escape E4D00A # citrine yellow
set -g fish_color_valid_path DFFF00 --underline # chartreuse yellow
set -g fish_color_operator 00CCFF # vivid sky blue
set -g fish_color_redirection 0ABAB5 # tiffany blue
set -g fish_color_end 007fff # azure blue
set -g fish_pager_color_background --background=normal
set -g fish_pager_color_prefix white --bold --underline
set -g fish_pager_color_completion 708238 # olive green
set -g fish_pager_color_description B2AC88 # sage green
set -g fish_pager_color_selected_background --background=normal
set -g fish_pager_color_selected_prefix green --bold --underline
set -g fish_pager_color_selected_completion yellow
set -g fish_pager_color_selected_description FEDC56 # mustard yellow
set -g fish_pager_color_progress --background=normal

# --ignore-glob (-I) <glob|glob>
function lla
  eza --all --long --group-directories-first --git --no-quotes \
    --no-permissions --octal-permissions --smart-group --time-style relative \
    --color always --color-scale size --color-scale-mode gradient $argv
end
function ll; lla --git-ignore $argv; end
function lll; ll --tree $argv; end
function llla; lla --tree $argv; end
# --exclude (-E) <glob>
# --type (-t) <file|dir>
function ff; fd --hidden --color always $argv; end
# --glob (-g) <!glob>
# --type (-t) <ext>; --type-not (-T) <ext>
# --invert-match (-v)
# --after-context (-A) <N>; --before-context (-B) <N>; --context (-C) <N>
function gg; rg --hidden --color always $argv; end
function vv
  bat --tabs 2 --map-syntax '*.ly:LaTeX' --style plain,header \
    --decorations always --color always --theme 1337 --paging always $argv
end
function ee; emacs -nw $argv; end

function fzf_history
  history --null | fzf --query=(commandline) --scheme=history \
    --read0 --print0 --cycle --bind=alt-m:down,alt-,:up --ansi | \
    read --null --local result; and commandline -- $result
end
bind \cR fzf_history

function fzf_view
  set -l file (fd --type file --hidden --exclude .git --exclude node_modules \
    --color always . | fzf --cycle --bind=alt-m:down,alt-,:up --ansi)
  bat --tabs=2 $file
end
bind \ew fzf_view

function fzf_open
  set -l file (fd --no-ignore -t file -e pdf -e djvu . \
    ~/Downloads ~/Projects/bayanguru |
    fzf --cycle --bind=alt-m:down,alt-,:up --ansi)
  nohup xdg-open $file &>/dev/null & disown $last_pid
end
bind \eo fzf_open

bind \em 'if commandline --paging-mode; commandline -f down-line; \
  else; commandline -f accept-autosuggestion; end'
bind \e, up-line

starship init fish | source
