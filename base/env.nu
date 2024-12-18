$env.PATH = $env.PATH | split row ":" | prepend ($env.HOME
  | path join .config/{go,emacs,lilypond,foundry}/bin | str expand)
$env.GOPATH = ($env.HOME | path join .config/go)
$env.EDITOR = "emacs -nw"
$env.PAGER = "less"
$env.LESS = "-RFQ --no-vbell"
$env.LS_COLORS = (vivid generate snazzy)

mkdir ~/.cache/starship
starship init nu | save -f ~/.cache/starship/init.nu
