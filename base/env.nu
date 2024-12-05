# $env.EDITOR = "emacs"
# $env.PATH = ($env.PATH | split row (char esep) | prepend "a/b")

# Starship prompt
mkdir ~/.cache/starship
starship init nu | save -f ~/.cache/starship/init.nu
