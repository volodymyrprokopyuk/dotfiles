$env.config = {
  show_banner: false,
  table: {mode: none},
  history: {
    file_format: sqlite, max_size: 10_000, isolation: true, sync_on_enter: true
  }
  keybindings: [{
    name: completion_next, modifier: alt, keycode: char_m, mode: [emacs],
    event: {until: [{send: historyhintcomplete}, {send: menunext}]}
  }, {
    name: completion_prev, modifier: alt, keycode: char_u002c, mode: [emacs],
    event: {send: menuprevious}
  }, {
    name: fzf_history, modifier: control, keycode: char_r, mode: [emacs],
    event: [{send: ExecuteHostCommand, cmd: fzf-history}]
  }, {
    name: fzf_view, modifier: alt, keycode: char_w, mode: [emacs],
    event: [{send: ExecuteHostCommand, cmd: fzf-view}]
  }, {
    name: fzf_open, modifier: alt, keycode: char_o, mode: [emacs],
    event: [{send: ExecuteHostCommand, cmd: fzf-open}]
  }]
}

# --ignore-glob (-I) <glob|glob>
alias lla = eza --all --long --group-directories-first --no-permissions --octal-permissions --smart-group --time-style relative --git --no-quotes --color always --color-scale size --color-scale-mode gradient
alias ll = lla --git-ignore
alias lll = ll --tree
# --exclude (-E) <glob>
# --type (-t) <file|dir>
alias ff = fd --hidden --color always
# --glob (-g) <!glob>
# --type (-t) <ext>
# --type-not (-T) <ext>
# --invert-match (-v)
# --after-context (-A) <N>
# --before-context (-B) <N>
# --context (-C) <N>
alias gg = rg --hidden --color always
alias vv = bat --tabs 2 --map-syntax '*.ly:LaTeX' --style plain,header --decorations always --color always --theme 1337 --paging always
alias ee = emacs -nw

alias fzf-base = fzf --cycle --bind alt-m:down,alt-,:up --ansi

def fzf-history [] {
  history | get command | uniq | reverse
    | each { nu-highlight } | str join (char --integer 0)
    | fzf-base --query (commandline) --scheme history --read0
    | commandline edit --replace $in
}

def fzf-view [] {
  ff --exclude .git | fzf-base | vv $in
}

def fzf-open [] {
  ff --no-ignore --type file '(pdf|djvu?)$' ~/Downloads ~/Projects/bayanguru
    | fzf-base | xdg-open $in o+e>| ignore
}

use ~/.cache/starship/init.nu

source ~/.config/nushell/vs-code-dark-plus.nu
