local wezterm = require("wezterm")
local config = wezterm.config_builder()

-- ===============
-- Basics
-- ===============
config.check_for_updates = false
config.automatically_reload_config = true
config.audible_bell = "Disabled"
config.scrollback_lines = 10000
config.adjust_window_size_when_changing_font_size = false

-- ===============
-- Font
-- ===============
config.font_size = 10.0
config.font = wezterm.font_with_fallback({
  "Fira Code",
  "Cascadia Code",
  "Noto Sans Mono",
  "JetBrains Mono",
})

-- ===============
-- Colors / appearance
-- ===============
config.color_scheme = 'Zenburn'
config.window_background_opacity = 0.95
config.window_decorations = "TITLE | RESIZE"
config.hide_tab_bar_if_only_one_tab = true

-- ===============
-- Cross-platform shell
-- ===============
if wezterm.target_triple:find("windows") then
  -- Use pwsh if available, otherwise cmd
  -- config.default_prog = { "pwsh.exe", "-NoLogo" }
  config.default_prog = { "pwsh.exe" }
else
  -- Linux/macOS
  config.default_prog = { os.getenv("SHELL") or "/bin/bash", "-l" }
end

-- ===============
-- Key bindings
-- ===============
config.leader = { key = "a", mods = "CTRL", timeout_milliseconds = 1000 }

config.keys = {
  -- Split panes
  { key = "|", mods = "LEADER|SHIFT", action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
  { key = "-", mods = "LEADER", action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }) },

  -- Pane navigation
  { key = "h", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Left") },
  { key = "j", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Down") },
  { key = "k", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Up") },
  { key = "l", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Right") },

  -- Resize panes
  { key = "H", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize({ "Left", 5 }) },
  { key = "J", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize({ "Down", 5 }) },
  { key = "K", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize({ "Up", 5 }) },
  { key = "L", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize({ "Right", 5 }) },

  -- Tabs
  { key = "c", mods = "LEADER", action = wezterm.action.SpawnTab("CurrentPaneDomain") },
  { key = "x", mods = "LEADER", action = wezterm.action.CloseCurrentPane({ confirm = true }) },
  { key = "n", mods = "LEADER", action = wezterm.action.ActivateTabRelative(1) },
  { key = "p", mods = "LEADER", action = wezterm.action.ActivateTabRelative(-1) },

  -- Copy / paste
  { key = "c", mods = "CTRL|SHIFT", action = wezterm.action.CopyTo("Clipboard") },
  { key = "v", mods = "CTRL|SHIFT", action = wezterm.action.PasteFrom("Clipboard") },

  -- Reload config
  { key = "r", mods = "LEADER", action = wezterm.action.ReloadConfiguration },
}

-- ===============
-- Optional: better behavior in Emacs and TUI apps
-- ===============
config.enable_kitty_keyboard = false
config.enable_csi_u_key_encoding = true
config.send_composed_key_when_left_alt_is_pressed = true
config.send_composed_key_when_right_alt_is_pressed = true

-- ===============
-- Useful on Windows
-- ===============
if wezterm.target_triple:find("windows") then
  -- config.default_domain = "WSL:Ubuntu" -- change if needed, or remove
  config.launch_menu = {
    { label = "PowerShell", args = { "pwsh.exe", "-NoLogo" } },
    { label = "Command Prompt", args = { "cmd.exe" } },
    -- { label = "WSL Ubuntu", args = { "wsl.exe", "-d", "Ubuntu" } },
  }
end

return config
