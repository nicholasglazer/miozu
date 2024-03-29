module Help (help) where

-- TODO make full list of bindings
help :: String
help = unlines
  [ "The default modifier key is 'mod4Mask' - windows key. Default keybindings:"
  , ""
  , "-- launching and killing programs"
  , "mod-Enter        Launch wizterm"
  , "mod-Shift-Enter  Launch wizterm"
  , "mod-d            Launch application launcher rofi"
  , "mod-Shift-c      Close/kill the focused window"
  , "mod-Space        Rotate through the available layout algorithms"
  , "mod-Shift-Space  Reset the layouts on the current workSpace to default"
  , "mod-Shift-n      Resize/refresh viewed windows to the correct size"
  , "mod-n            Launch NetworkManager applet"
  , ""
  , "-- move focus up or down the window stack"
  , "mod-Tab        Move focus to the next window"
  , "mod-Shift-Tab  Move focus to the previous window"
  , "mod-j          Move focus to the next window"
  , "mod-k          Move focus to the previous window"
  , "mod-m          Move focus to the master window"
  , ""
  , "-- modifying the window order"
  , "mod-Return   Swap the focused window and the master window"
  , "mod-Shift-j  Swap the focused window with the next window"
  , "mod-Shift-k  Swap the focused window with the previous window"
  , ""
  , "-- resizing the master/slave ratio"
  , "mod-h  Shrink the master area"
  , "mod-l  Expand the master area"
  , ""
  , "-- increase or decrease number of windows in the master area"
  , "mod-comma  (mod-,)   Increment the number of windows in the master area"
  , "mod-period (mod-.)   Deincrement the number of windows in the master area"
  , ""
  , "-- quit, or restart"
  , "mod-Shift-apostrophe  Quit xmonad"
  , "mod-apostrophe        Restart xmonad"
  , ""
  , "-- Workspaces & screens"
  , "mod-[1..9]                  Switch to workSpace N"
  , "mod-Shift-[1..9]            Move client to workspace N"
  , "mod-mod-{comma,period,p}    witch to physical/Xinerama screens 1, 2, or 3"
  , "mod-Shift-{comma,period,p}  Move client to screen 1, 2, or 3"
  , ""
  , "-- Mouse bindings: default actions bound to mouse events"
  , "mod-button1  Set the window to floating mode and move by dragging"
  , "mod-button2  Raise the window to the top of the stack"
  , "mod-button3  Set the window to floating mode and resize by dragging"
  ]
