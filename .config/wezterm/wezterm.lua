-- Last updated Aug 30 2024
-- Nicholas Glazer <glazer.nicholas@gmail.com>
--
-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

---- User configs ----
-- Miozu dark color scheme
config.color_scheme = 'Miozu'

-- Hide tab bar if there is only one tab
config.hide_tab_bar_if_only_one_tab = true

-- Prevent Wezterm from asking you if you want to close the terminal when there are active sessions or running processes.
config.window_close_confirmation = 'NeverPrompt'

--default font is "JetBrains Mono"
config.font = wezterm.font("SourceCodePro")
config.font_size = 12
config.freetype_load_target = "Light"
config.freetype_render_target = "HorizontalLcd"

-- and finally, return the configuration to wezterm
return config
