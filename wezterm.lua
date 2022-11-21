local wezterm = require "wezterm"

-- Maximize window on start up
wezterm.on("gui-startup", function(cmd)
  local tab, pane, window = wezterm.mux.spawn_window(cmd or { })
  window:gui_window():maximize()
end)

return {
  -- Font
  font = wezterm.font("JetBrainsMono Nerd Font Mono", { weight = "Light" }),
  font_size = 10,
  -- Color theme
  -- color_scheme = "zenwritten_dark",
  -- color_scheme = "wilmersdorf",
  -- color_scheme = "Wombat",
  -- color_scheme = "Vacuous 2 (terminal.sexy)",
  -- color_scheme = "VisiBone (terminal.sexy)",
  color_scheme = "VSCodeDark+ (Gogh)",
  colors = {
    -- Transparent cursor
    cursor_fg = "rgba(0,0,0,1)",
  },
  keys = {
    -- Toggle full screen
    { key = "F11", action = wezterm.action.ToggleFullScreen },
  },
}
