-- WezTerm Configuration with Miozu Theme and Vim Support
-- Nicholas Glazer <glazer.nicholas@gmail.com>

local wezterm = require 'wezterm'
local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

---- Theme Configuration ----
config.color_scheme = 'Miozu'

---- Font Configuration ----
config.font = wezterm.font("SourceCodePro")
config.font_size = 12
config.freetype_load_target = "Light"
config.freetype_render_target = "HorizontalLcd"

---- Window Configuration ----
config.hide_tab_bar_if_only_one_tab = true
config.window_close_confirmation = 'NeverPrompt'
config.window_background_opacity = 0.98

---- Vim Integration ----
config.enable_kitty_keyboard = true  -- Better keyboard support for vim
config.enable_csi_u_key_encoding = true  -- Better unicode support

---- Performance Optimizations ----
config.max_fps = 120
config.animation_fps = 60
config.front_end = "WebGpu"  -- Use GPU acceleration
config.webgpu_power_preference = "HighPerformance"

---- Scrollback ----
config.scrollback_lines = 10000

---- Bell Configuration ----
config.audible_bell = "Disabled"
config.visual_bell = {
  fade_in_duration_ms = 75,
  fade_out_duration_ms = 75,
  target = "CursorColor",
}

---- Copy/Paste Behavior ----
config.selection_word_boundary = " \t\n{}[]()\"'`,;:"

return config
