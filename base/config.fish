set -gx GOPATH $HOME/.local/go
set -gx PATH $GOPATH/bin $HOME/.config/{emacs,lilypond,foundry}/bin $PATH
set -gx EDITOR emacs -nw
set -gx PAGER less
set -gx LESS ' --incsearch --RAW-CONTROL-CHARS --QUIET --no-vbell'
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
# --level L <depth>
function lla
  eza --all --long --group-directories-first --git --no-quotes \
    --no-permissions --octal-permissions --smart-group --time-style relative \
    --color always --color-scale size --color-scale-mode gradient $argv
end
function ll
  lla --git-ignore $argv
end
function lll
  ll --tree $argv
end
function llla
  lla --tree $argv
end
# --exclude (-E) <glob>
# --type (-t) <file|dir>
function ff
  fd --hidden --color always $argv
end
# --glob (-g) <!glob>
# --type (-t) <ext>; --type-not (-T) <ext>
# --invert-match (-v)
# --after-context (-A) <N>; --before-context (-B) <N>; --context (-C) <N>
function gg
  rg --hidden --color always $argv
end
function vv
  bat --tabs 2 --map-syntax '*.ly:LaTeX' --style plain,header \
    --decorations always --color always --theme 1337 --paging always $argv
end
function ee
  emacs -nw $argv
end
function jj
  yq --input-format json --output-format json --prettyPrint --indent 2 $arvg
end
function yy
  yq --input-format yaml --output-format yaml --prettyPrint --indent 2 $arvg
end

function fzfBase
  fzf --cycle --bind alt-m:down,alt-,:up --ansi $argv
end

function fzfHistory
  history --null |
  fzfBase --query=(commandline) --scheme=history --read0 --print0 |
  read --null --local selected; and commandline -- $selected
end
bind ctrl-r fzfHistory

function fzfView
  ff --exclude .git | fzfBase | read --local selected; and vv $selected
end
bind alt-w fzfView

function fzfDiff
  git l --color=always | fzfBase --reverse | read --local selected
  and git d --color=always (string match --regex '\w{7}' $selected)^! | delta | less
end
bind alt-i fzfDiff

function fzfOpen
  ff --no-ignore --type file '(pdf|djvu?)$' ~/Downloads ~/Projects/bayanguru |
    fzfBase | read --local selected; and xdg-open $selected &>/dev/null
end
bind alt-o fzfOpen

bind alt-m 'if commandline --paging-mode; commandline -f down-line; \
  else; commandline -f accept-autosuggestion; end'
bind alt-comma up-line

starship init fish | source

set -gx _ZO_FZF_OPTS '--cycle --bind alt-m:down,alt-,:up --ansi'
zoxide init --cmd j fish | source
