$env.EDITOR = "emacs"
$env.PATH = ($env.PATH | split row (char esep) | prepend "a/b")
