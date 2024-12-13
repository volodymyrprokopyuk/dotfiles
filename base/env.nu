$env.PATH = $env.PATH | split row ":"
  | prepend (
    ["go", "emacs", "lilypond", "foundry"]
    | each { $"($env.HOME)/.config/($in)/bin" }
  )
$env.GOPATH = $"($env.HOME)/.config/go"
$env.EDITOR = ["emacs", "-nw"]
$env.PAGER = "less"
$env.LESS = "-RFQ --no-vbell"
$env.LS_COLORS = (vivid generate snazzy)

mkdir ~/.cache/starship
starship init nu | save -f ~/.cache/starship/init.nu
