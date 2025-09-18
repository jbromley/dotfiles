local wezterm = require("wezterm")
local act = wezterm.action

-- Customize key bindings.
my_keys ={
    { key = 'p',          mods = 'SHIFT|CTRL', action = act.ActivateCommandPalette },
    { key = 'x',          mods = 'SHIFT|CTRL', action = act.ActivateCopyMode },

    { key = 'DownArrow',  mods = 'SHIFT',      action = act.ActivatePaneDirection 'Down' },
    { key = 'LeftArrow',  mods = 'SHIFT',      action = act.ActivatePaneDirection 'Left' },
    { key = 'RightArrow', mods = 'SHIFT',      action = act.ActivatePaneDirection 'Right' },
    { key = 'UpArrow',    mods = 'SHIFT',      action = act.ActivatePaneDirection 'Up' },

    { key = '(',          mods = 'SHIFT|CTRL', action = act.ActivateTab(-1) },
    { key = '1',          mods = 'LEADER',     action = act.ActivateTab(0) },
    { key = '2',          mods = 'LEADER',     action = act.ActivateTab(1) },
    { key = '3',          mods = 'LEADER',     action = act.ActivateTab(2) },
    { key = '4',          mods = 'LEADER',     action = act.ActivateTab(3) },
    { key = '5',          mods = 'LEADER',     action = act.ActivateTab(4) },
    { key = '6',          mods = 'LEADER',     action = act.ActivateTab(5) },
    { key = '7',          mods = 'LEADER',     action = act.ActivateTab(6) },
    { key = '8',          mods = 'LEADER',     action = act.ActivateTab(7) },
    { key = 'PageUp',     mods = 'CTRL',       action = act.ActivateTabRelative(-1) },
    { key = 'Tab',        mods = 'SHIFT|CTRL', action = act.ActivateTabRelative(-1) },
    { key = '[',          mods = 'LEADER',     action = act.ActivateTabRelative(-1) },
    { key = 'PageDown',   mods = 'CTRL',       action = act.ActivateTabRelative(1) },
    { key = 'Tab',        mods = 'CTRL',       action = act.ActivateTabRelative(1) },
    { key = ']',          mods = 'LEADER',     action = act.ActivateTabRelative(1) },

    { key = 'DownArrow',  mods = 'CTRL',   action = act.AdjustPaneSize{ 'Down', 1 } },
    { key = 'LeftArrow',  mods = 'CTRL',   action = act.AdjustPaneSize{ 'Left', 1 } },
    { key = 'RightArrow', mods = 'CTRL',   action = act.AdjustPaneSize{ 'Right', 1 } },
    { key = 'UpArrow',    mods = 'CTRL',   action = act.AdjustPaneSize{ 'Up', 1 } },

    { key = 'u',          mods = 'LEADER', action = act.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
    { key = 'k',          mods = 'LEADER', action = act.ClearScrollback 'ScrollbackOnly' },
    { key = 'w',          mods = 'LEADER', action = act.CloseCurrentTab{ confirm = true } },

    { key = 'c',          mods = 'LEADER', action = act.CopyTo 'Clipboard' },
    { key = 'Copy',       mods = 'NONE',       action = act.CopyTo 'Clipboard' },
    { key = 'Insert',     mods = 'CTRL',       action = act.CopyTo 'PrimarySelection' },

    { key = 'v',          mods = 'LEADER', action = act.PasteFrom 'Clipboard' },
    { key = 'Paste',      mods = 'NONE',       action = act.PasteFrom 'Clipboard' },
    { key = 'Insert',     mods = 'SHIFT',      action = act.PasteFrom 'PrimarySelection' },

    { key = '-',          mods = 'LEADER', action = act.DecreaseFontSize },
    { key = '=',          mods = 'LEADER', action = act.IncreaseFontSize },
    { key = '0',          mods = 'LEADER', action = act.ResetFontSize },

    { key = 'm',          mods = 'LEADER', action = act.Hide },

    { key = 'PageUp',     mods = 'LEADER', action = act.MoveTabRelative(-1) },
    { key = 'PageDown',   mods = 'LEADER', action = act.MoveTabRelative(1) },

    { key = 'phys:Space', mods = 'LEADER', action = act.QuickSelect },

    { key = 'r',          mods = 'LEADER', action = act.ReloadConfiguration },

    { key = 'PageUp',     mods = 'SHIFT',      action = act.ScrollByPage(-1) },
    { key = 'PageDown',   mods = 'SHIFT',      action = act.ScrollByPage(1) },

    { key = 'f',          mods = 'LEADER', action = act.Search 'CurrentSelectionOrEmptyString' },

    { key = 'l',          mods = 'LEADER', action = act.ShowDebugOverlay },

    { key = 't',          mods = 'LEADER', action = act.SpawnTab 'CurrentPaneDomain' },
    { key = 'n',          mods = 'LEADER', action = act.SpawnWindow },

    { key = '5',          mods = 'ALT|CTRL',   action = act.SplitHorizontal{ domain =  'CurrentPaneDomain' } },
    { key = '\'',         mods = 'ALT|CTRL',   action = act.SplitVertical{ domain =  'CurrentPaneDomain' } },

    { key = 'Enter',      mods = 'LEADER',        action = act.ToggleFullScreen },
    { key = 'z',          mods = 'LEADER', action = act.TogglePaneZoomState },
}

my_key_tables = {
  copy_mode = {
    { key = 'g', mods = 'NONE', action = act.CopyMode 'MoveToScrollbackTop' },
    { key = 'u', mods = 'CTRL', action = act.CopyMode{ MoveByPage = (-0.5) } },
    { key = '0', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLine' },
    { key = '^', mods = 'SHIFT', action = act.CopyMode 'MoveToStartOfLineContent' },
    { key = 'b', mods = 'NONE', action = act.CopyMode 'MoveBackwardWord' },
    { key = 'w', mods = 'NONE', action = act.CopyMode 'MoveForwardWord' },
    { key = 'e', mods = 'NONE', action = act.CopyMode 'MoveForwardWordEnd' },
    { key = '$', mods = 'SHIFT', action = act.CopyMode 'MoveToEndOfLineContent' },
    { key = 'Enter', mods = 'NONE', action = act.CopyMode 'MoveToStartOfNextLine' },
    { key = 'd', mods = 'CTRL', action = act.CopyMode{ MoveByPage = (0.5) } },
    { key = 'g', mods = 'SHIFT', action = act.CopyMode 'MoveToScrollbackBottom' },
    { key = 'Escape', mods = 'NONE', action = act.Multiple{ 'ScrollToBottom', { CopyMode =  'Close' } } },
    { key = 'g', mods = 'CTRL', action = act.Multiple{ 'ScrollToBottom', { CopyMode =  'Close' } } },
    { key = 'h', mods = 'NONE', action = act.CopyMode 'MoveLeft' },
    { key = 'j', mods = 'NONE', action = act.CopyMode 'MoveDown' },
    { key = 'k', mods = 'NONE', action = act.CopyMode 'MoveUp' },
    { key = 'l', mods = 'NONE', action = act.CopyMode 'MoveRight' },
    { key = 'o', mods = 'SHIFT', action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
    { key = 'o', mods = 'NONE', action = act.CopyMode 'MoveToSelectionOtherEnd' },
    { key = 'PageUp', mods = 'NONE', action = act.CopyMode 'PageUp' },
    { key = 'u', mods = 'SHIFT|CTRL', action = act.CopyMode 'PageUp' },
    { key = 'd', mods = 'SHIFT|CTRL', action = act.CopyMode 'PageDown' },
    { key = 'PageDown', mods = 'NONE', action = act.CopyMode 'PageDown' },
    { key = 'Space', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
    { key = 'v', mods = 'SHIFT', action = act.CopyMode{ SetSelectionMode =  'Line' } },
    { key = 'v', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
    { key = 'v', mods = 'CTRL', action = act.CopyMode{ SetSelectionMode =  'Block' } },
    { key = ';', mods = 'NONE', action = act.CopyMode 'JumpAgain' },
    { key = 'f', mods = 'SHIFT', action = act.CopyMode{ JumpBackward = { prev_char = false } } },
    { key = 't', mods = 'SHIFT', action = act.CopyMode{ JumpBackward = { prev_char = true } } },
    { key = 'f', mods = 'NONE', action = act.CopyMode{ JumpForward = { prev_char = false } } },
    { key = 't', mods = 'NONE', action = act.CopyMode{ JumpForward = { prev_char = true } } },
    { key = 'h', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportTop' },
    { key = 'l', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportBottom' },
    { key = 'm', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportMiddle' },
    { key = 'q', mods = 'NONE', action = act.Multiple{ 'ScrollToBottom', { CopyMode =  'Close' } } },
    { key = 'y', mods = 'NONE', action = act.Multiple{ { CopyTo =  'ClipboardAndPrimarySelection' }, { Multiple = { 'ScrollToBottom', { CopyMode =  'Close' } } } } },
  },
  search_mode = {
    { key = 'Enter', mods = 'NONE', action = act.CopyMode 'PriorMatch' },
    { key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
    { key = 'n', mods = 'CTRL', action = act.CopyMode 'NextMatch' },
    { key = 'p', mods = 'CTRL', action = act.CopyMode 'PriorMatch' },
    { key = 'r', mods = 'CTRL', action = act.CopyMode 'CycleMatchType' },
    { key = 'u', mods = 'CTRL', action = act.CopyMode 'ClearPattern' },
    { key = 'PageUp', mods = 'NONE', action = act.CopyMode 'PriorMatchPage' },
    { key = 'PageDown', mods = 'NONE', action = act.CopyMode 'NextMatchPage' },
    { key = 'UpArrow', mods = 'NONE', action = act.CopyMode 'PriorMatch' },
    { key = 'DownArrow', mods = 'NONE', action = act.CopyMode 'NextMatch' },
  },
}

return {
	term = "wezterm",
	initial_cols = 120,
	initial_rows = 50,
	color_scheme = "Dracula (Official)",

	window_frame = {
		font_size = 10.0,
	},

	font = wezterm.font("Atkinson Hyperlegible Mono"),
	font_size = 10.0,

	tab_bar_at_bottom = true,
	tab_max_width = 256,

	window_decorations = "RESIZE",
	window_background_opacity = 0.90,

	inactive_pane_hsb = {
		saturation = 0.9,
		brightness = 0.9,
	},

	disable_default_key_bindings = true,
	leader = { key = "VoidSymbol", mods = "", timeout_milliseconds = 500 },
	keys = my_keys,
	key_tables = my_key_tables,
}

-- vim: foldmethod=marker:foldlevel=10
