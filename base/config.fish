starship init fish | source

function ll
  eza --all --git-ignore --sort=type --long --git \
    --time-style=relative  --smart-group --no-permissions --octal-permissions \
    --color-scale --no-quotes $argv
end

function lll
  ll --tree $argv
end
