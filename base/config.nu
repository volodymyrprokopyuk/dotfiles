$env.config = {
  show_banner: false,
  # buffer_editor: emacs
}

$env.LS_COLORS = (vivid generate snazzy | str trim)

# Starship prompt
use ~/.cache/starship/init.nu
