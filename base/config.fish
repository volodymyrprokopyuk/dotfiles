starship init fish | source

function ll
  eza --all --git-ignore --sort=type --long --git \
    --time-style=relative  --smart-group --no-permissions --octal-permissions \
    --color-scale --no-quotes $argv
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

function ee
  emacs -nw $argv
end
