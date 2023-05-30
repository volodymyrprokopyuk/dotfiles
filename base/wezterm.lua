local wezterm = require "wezterm"
local act = wezterm.action

-- Maximize window on start up
wezterm.on("gui-startup", function(cmd)
  local tab, pane, window = wezterm.mux.spawn_window(cmd or { })
  window:gui_window():maximize()
end)

return {
  -- Appearence
  hide_tab_bar_if_only_one_tab = true,
  -- Font
  font = wezterm.font("JetBrainsMono Nerd Font Mono", { weight = "Light" }),
  font_size = 13,
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
    copy_mode_inactive_highlight_bg = { Color = "#007915" },
    -- Copy
    selection_fg = "#FFDF85",
    selection_bg = "#480000",
    -- Quick select
    quick_select_label_fg = { Color = "#FFDF85" },
    quick_select_label_bg = { Color = "#EF1100" },
    quick_select_match_fg = { Color = "#FFDF85" },
    quick_select_match_bg = { Color = "#480000" },
  },
  inactive_pane_hsb = { saturation = 0.9, brightness = 0.7 },
  scrollback_lines = 10000,
  -- Key bindings
  disable_default_key_bindings = true,
  leader = { key = " ", mods = "ALT", timeout_milliseconds = 1000 },
  keys = {
    -- Toggle full screen
    { key = "F11", mods = "NONE", action = act.ToggleFullScreen },
    -- Font size
    { key = "+", mods = "LEADER|SHIFT", action = act.IncreaseFontSize },
    { key = "-", mods = "LEADER", action = act.DecreaseFontSize },
    { key = "0", mods = "LEADER", action = act.ResetFontSize },
    -- Tabs
    { key = "n", mods = "LEADER|SHIFT", action = act.SpawnTab "CurrentPaneDomain" },
    { key = "}", mods = "LEADER|SHIFT", action = act.ActivateTabRelative(1) },
    { key = "{", mods = "LEADER|SHIFT", action = act.ActivateTabRelative(-1) },
    { key = "o", mods = "LEADER|SHIFT", action = act.ActivateLastTab },
    { key = "t", mods = "LEADER|SHIFT", action = act.ShowTabNavigator },
    { key = "q", mods = "LEADER|SHIFT", action = act.CloseCurrentTab { confirm = true } },
    -- Panes
    { key = "v", mods = "ALT", action = act.SplitHorizontal { domain = "CurrentPaneDomain" } },
    { key = "s", mods = "ALT", action = act.SplitVertical { domain = "CurrentPaneDomain" } },
    { key = "z", mods = "ALT", action = act.TogglePaneZoomState },
    { key = "r", mods = "ALT", action = act.RotatePanes "Clockwise" },
    { key = "g", mods = "ALT", action = act.PaneSelect },
    { key = "h", mods = "ALT", action = act.ActivatePaneDirection "Left" },
    { key = "j", mods = "ALT", action = act.ActivatePaneDirection "Down" },
    { key = "k", mods = "ALT", action = act.ActivatePaneDirection "Up" },
    { key = "l", mods = "ALT", action = act.ActivatePaneDirection "Right" },
    { key = "q", mods = "ALT", action = act.CloseCurrentPane { confirm = true } },
    -- Scroll
    -- { key = "e", mods = "CTRL", action = act.ScrollByLine(5) },
    -- { key = "y", mods = "CTRL", action = act.ScrollByLine(-5) },
    -- { key = "f", mods = "CTRL", action = act.ScrollByPage(1.0) },
    -- { key = "b", mods = "CTRL", action = act.ScrollByPage(-1.0) },
    -- { key = "g", mods = "SHIFT", action = act.ScrollToBottom },
    -- Search
    { key = "/", mods = "ALT", action = act.Search "CurrentSelectionOrEmptyString" },
    -- Copy
    { key = "y", mods = "ALT", action = act.ActivateCopyMode },
    -- Paste
    { key = "p", mods = "ALT", action = act.PasteFrom "Clipboard" },
    { key = "p", mods = "LEADER|SHIFT", action = act.PasteFrom "PrimarySelection" },
    -- Quick select
    { key = "?", mods = "LEADER|SHIFT", action = act.QuickSelect },
  },
  key_tables = {
    search_mode = {
      { key = "Enter", mods = "NONE", action = act.CopyMode "PriorMatch" },
      { key = "k", mods = "ALT", action = act.CopyMode "PriorMatch" },
      { key = "j", mods = "ALT", action = act.CopyMode "NextMatch" },
      { key = "m", mods = "CTRL", action = act.CopyMode "CycleMatchType" },
      { key = "c", mods = "CTRL", action = act.CopyMode "ClearPattern" },
      { key = "g", mods = "CTRL", action = act.CopyMode "Close" },
    },
    copy_mode = {
      { key = "h", mods = "NONE", action = act.CopyMode "MoveLeft" },
      { key = "j", mods = "NONE", action = act.CopyMode "MoveDown" },
      { key = "k", mods = "NONE", action = act.CopyMode "MoveUp" },
      { key = "l", mods = "NONE", action = act.CopyMode "MoveRight" },
      { key = "w", mods = "NONE", action = act.CopyMode "MoveForwardWord" },
      { key = "b", mods = "NONE", action = act.CopyMode "MoveBackwardWord" },
      { key = "$", mods = "SHIFT", action = act.CopyMode "MoveToEndOfLineContent" },
      { key = "^", mods = "SHIFT", action = act.CopyMode "MoveToStartOfLineContent" },
      { key = "0", mods = "NONE", action = act.CopyMode "MoveToStartOfLine" },
      { key = "f", mods = "CTRL", action = act.CopyMode "PageDown" },
      { key = "b", mods = "CTRL", action = act.CopyMode "PageUp" },
      { key = "g", mods = "NONE", action = act.CopyMode "MoveToScrollbackTop" },
      { key = "G", mods = "SHIFT", action = act.CopyMode "MoveToScrollbackBottom" },
      { key = "h", mods = "SHIFT", action = act.CopyMode "MoveToViewportTop" },
      { key = "m", mods = "SHIFT", action = act.CopyMode "MoveToViewportMiddle" },
      { key = "l", mods = "SHIFT", action = act.CopyMode "MoveToViewportBottom" },
      { key = "v", mods = "NONE", action = act.CopyMode { SetSelectionMode = "Cell" } },
      { key = "v", mods = "SHIFT", action = act.CopyMode { SetSelectionMode = "Line" } },
      { key = "v", mods = "CTRL", action = act.CopyMode { SetSelectionMode = "Block" } },
      { key = "o", mods = "NONE", action = act.CopyMode "MoveToSelectionOtherEndHoriz" },
      { key = "y", mods = NONE, action = act.Multiple {
        { CopyTo = "ClipboardAndPrimarySelection" }, { CopyMode = "Close" } } },
      { key = "g", mods = "CTRL", action = act.CopyMode "Close" },
    },
  },
}
