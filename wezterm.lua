local wezterm = require "wezterm"
local action = wezterm.action

-- Maximize window on start up
wezterm.on("gui-startup", function(cmd)
  local tab, pane, window = wezterm.mux.spawn_window(cmd or { })
  window:gui_window():maximize()
end)

return {
  -- General
  hide_tab_bar_if_only_one_tab = true,
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
    -- Search
    copy_mode_active_highlight_fg = { Color = "#FFDF85" },
    copy_mode_active_highlight_bg = { Color = "#EF1100" },
    copy_mode_inactive_highlight_fg = { Color = "#FFDF85" },
    copy_mode_inactive_highlight_bg = { Color = "007915" },
  },
  inactive_pane_hsb = { daturation = 0.7, brightness = 0.7 },
  scrollback_lines = 10000,
  -- Key bindings
  disable_default_key_bindings = true,
  leader = { key = " ", mods = "ALT", timeout_milliseconds = 1000 },
  keys = {
    -- Toggle full screen
    { key = "F11", mods = "NONE", action = action.ToggleFullScreen },
    -- Font size
    { key = "+", mods = "LEADER|SHIFT", action = action.IncreaseFontSize },
    { key = "-", mods = "LEADER", action = action.DecreaseFontSize },
    { key = "0", mods = "LEADER", action = action.ResetFontSize },
    -- Tabs
    { key = "n", mods = "LEADER|SHIFT", action = action.SpawnTab "CurrentPaneDomain" },
    { key = "}", mods = "LEADER|SHIFT", action = action.ActivateTabRelative(1) },
    { key = "{", mods = "LEADER|SHIFT", action = action.ActivateTabRelative(-1) },
    { key = "o", mods = "LEADER|SHIFT", action = action.ActivateLastTab },
    { key = "t", mods = "LEADER|SHIFT", action = action.ShowTabNavigator },
    { key = "q", mods = "LEADER|SHIFT",
      action = action.CloseCurrentTab { confirm = true } },
    -- Panes
    { key = "v", mods = "LEADER",
      action = action.SplitHorizontal { domain = "CurrentPaneDomain" } },
    { key = "s", mods = "LEADER",
      action = action.SplitVertical { domain = "CurrentPaneDomain" } },
    { key = "z", mods = "LEADER", action = action.TogglePaneZoomState },
    { key = "r", mods = "LEADER", action = action.RotatePanes "Clockwise" },
    { key = "g", mods = "LEADER", action = action.PaneSelect },
    { key = "h", mods = "LEADER", action = action.ActivatePaneDirection "Left" },
    { key = "j", mods = "LEADER", action = action.ActivatePaneDirection "Down" },
    { key = "k", mods = "LEADER", action = action.ActivatePaneDirection "Up" },
    { key = "l", mods = "LEADER", action = action.ActivatePaneDirection "Right" },
    { key = "q", mods = "LEADER",
      action = action.CloseCurrentPane { confirm = true } },
    -- Scroll
    { key = "f", mods = "CTRL", action = action.ScrollByPage(0.5) },
    { key = "b", mods = "CTRL", action = action.ScrollByPage(-0.5) },
    { key = "e", mods = "CTRL", action = action.ScrollByLine(5) },
    { key = "y", mods = "CTRL", action = action.ScrollByLine(-5) },
    { key = "g", mods = "SHIFT", action = action.ScrollToBottom },
    -- Search
    { key = "/", mods = "LEADER",
      action = action.Search "CurrentSelectionOrEmptyString" },
  },
  key_tables = {
    search_mode = {
      { key = "Enter", mods = "NONE", action = action.CopyMode "PriorMatch" },
      { key = "k", mods = "ALT", action = action.CopyMode "PriorMatch" },
      { key = "j", mods = "ALT", action = action.CopyMode "NextMatch" },
      { key = "m", mods = "CTRL", action = action.CopyMode "CycleMatchType" },
      { key = "c", mods = "CTRL", action = action.CopyMode "ClearPattern" },
      { key = "g", mods = "CTRL", action = action.CopyMode "Close" },
    },
  },
}
