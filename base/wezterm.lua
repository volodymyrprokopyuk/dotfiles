local wezterm = require "wezterm"
local act = wezterm.action

return {
  -- General
  default_prog = {"/usr/bin/fish"},
  hide_tab_bar_if_only_one_tab = true,
  scrollback_lines = 2000,
  -- Font
  font = wezterm.font("JetBrainsMono NF Light", {weight = "Light"}),
  font_size = 13,
  -- Color theme
  color_scheme = "VSCodeDark+ (Gogh)",
  colors = {
    -- Transparent cursor
    cursor_fg = "rgba(0,0,0,1)",
    -- Pane split
    split = "#EEC500",
    -- Search
    copy_mode_active_highlight_fg = {Color = "#FFDF85"},
    copy_mode_active_highlight_bg = {Color = "#EF1100"},
    copy_mode_inactive_highlight_fg = {Color = "#FFDF85"},
    copy_mode_inactive_highlight_bg = {Color = "#017371"},
    -- Copy
    selection_fg = "#FFDF85",
    selection_bg = "#480000",
  },
  -- Key bindings
  disable_default_key_bindings = true,
  leader = {key = " ", mods = "ALT", timeout_milliseconds = 1000},
  keys = {
    -- Toggle full screen
    {key = "F11", mods = "NONE", action = act.ToggleFullScreen},
    -- Font size
    {key = "+", mods = "ALT|SHIFT", action = act.IncreaseFontSize},
    {key = "-", mods = "ALT", action = act.DecreaseFontSize},
    {key = "=", mods = "ALT", action = act.ResetFontSize},
    -- Tabs
    {key = "n", mods = "LEADER", action = act.SpawnTab "CurrentPaneDomain"},
    {key = "]", mods = "LEADER", action = act.ActivateTabRelative(1)},
    {key = "[", mods = "LEADER", action = act.ActivateTabRelative(-1)},
    {key = "o", mods = "LEADER", action = act.ActivateLastTab},
    {key = "q", mods = "LEADER", action = act.CloseCurrentTab {confirm = true}},
    -- Panes
    {key = "v", mods = "ALT", action = act.SplitHorizontal {domain = "CurrentPaneDomain"}},
    {key = "s", mods = "ALT", action = act.SplitVertical {domain = "CurrentPaneDomain"}},
    {key = "z", mods = "ALT", action = act.TogglePaneZoomState},
    {key = "r", mods = "ALT", action = act.RotatePanes "Clockwise"},
    {key = ".", mods = "ALT", action = act.PaneSelect},
    {key = "h", mods = "ALT", action = act.ActivatePaneDirection "Left"},
    {key = "j", mods = "ALT", action = act.ActivatePaneDirection "Down"},
    {key = "k", mods = "ALT", action = act.ActivatePaneDirection "Up"},
    {key = "l", mods = "ALT", action = act.ActivatePaneDirection "Right"},
    {key = "q", mods = "ALT", action = act.CloseCurrentPane { confirm = true}},
    -- Scroll
    {key = "e", mods = "ALT", action = act.ScrollByLine(1)},
    {key = "y", mods = "ALT", action = act.ScrollByLine(-1)},
    {key = "d", mods = "ALT", action = act.ScrollByPage(0.9)},
    {key = "u", mods = "ALT", action = act.ScrollByPage(-0.9)},
    {key = "g", mods = "ALT", action = act.ScrollToBottom},
    -- Search
    {key = "/", mods = "ALT", action = act.Search "CurrentSelectionOrEmptyString"},
    -- Copy
    {key = "c", mods = "ALT", action = act.ActivateCopyMode},
    -- Paste
    {key = "p", mods = "ALT", action = act.PasteFrom "Clipboard"},
  },
  key_tables = {
    search_mode = {
      {key = "Enter", mods = "NONE", action = act.CopyMode "PriorMatch"},
      {key = "p", mods = "ALT", action = act.CopyMode "PriorMatch"},
      {key = "n", mods = "ALT", action = act.CopyMode "NextMatch"},
      {key = "c", mods = "ALT", action = act.CopyMode "ClearPattern"},
      {key = "g", mods = "CTRL", action = act.Multiple {
        act.CopyMode "ClearPattern",
        act.CopyMode "Close",
      }},
    },
    copy_mode = {
      {key = "h", mods = "NONE", action = act.CopyMode "MoveLeft"},
      {key = "j", mods = "NONE", action = act.CopyMode "MoveDown"},
      {key = "k", mods = "NONE", action = act.CopyMode "MoveUp"},
      {key = "l", mods = "NONE", action = act.CopyMode "MoveRight"},
      {key = "w", mods = "NONE", action = act.CopyMode "MoveForwardWord"},
      {key = "b", mods = "NONE", action = act.CopyMode "MoveBackwardWord"},
      {key = "e", mods = "NONE", action = act.CopyMode "MoveForwardWordEnd"},
      {key = "$", mods = "SHIFT", action = act.CopyMode "MoveToEndOfLineContent"},
      {key = "^", mods = "SHIFT", action = act.CopyMode "MoveToStartOfLineContent"},
      {key = "0", mods = "NONE", action = act.CopyMode "MoveToStartOfLine"},
      {key = "g", mods = "NONE", action = act.CopyMode "MoveToScrollbackTop"},
      {key = "g", mods = "SHIFT", action = act.CopyMode "MoveToScrollbackBottom"},
      {key = "f", mods = "NONE", action = act.CopyMode { JumpForward = {prev_char = false}}},
      {key = "f", mods = "SHIFT", action = act.CopyMode { JumpBackward = {prev_char = false}}},
      {key = "t", mods = "NONE", action = act.CopyMode { JumpForward = {prev_char = true}}},
      {key = "t", mods = "SHIFT", action = act.CopyMode { JumpBackward = {prev_char = true}}},
      {key = ";", mods = "NONE", action = act.CopyMode "JumpAgain"},
      {key = ",", mods = "NONE", action = act.CopyMode "JumpReverse"},
      {key = "v", mods = "NONE", action = act.CopyMode {SetSelectionMode = "Cell"}},
      {key = "v", mods = "SHIFT", action = act.CopyMode {SetSelectionMode = "Line"}},
      {key = "v", mods = "CTRL", action = act.CopyMode {SetSelectionMode = "Block"}},
      {key = "o", mods = "NONE", action = act.CopyMode "MoveToSelectionOtherEndHoriz"},
      {key = "o", mods = "SHIFT", action = act.CopyMode "MoveToSelectionOtherEnd"},
      {key = "y", mods = "NONE", action = act.Multiple {
        act.CopyTo "ClipboardAndPrimarySelection",
        act.CopyMode "ClearSelectionMode",
        act.CopyMode "Close",
      }},
      {key = "g", mods = "CTRL", action = act.Multiple{
        act.CopyMode "ClearSelectionMode",
        act.CopyMode "Close",
      }},
    },
  },
}
