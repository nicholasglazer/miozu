------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
-- Run in termilan to find needed key:
-- xev | fgrep "keysym"
------------------------------------------------------------------------
module Keybindings (myKeys) where

import XMonad
import System.Process
import System.Exit (exitWith, ExitCode(ExitSuccess))
-- Import /lib modules
import Scratchpads (scratchTermSL, scratchTermBL, scratchTermSR, scratchTermBR, scratchFM, scratchWebA, scratchWebB)
import ScreenRecorder (myGifRecorder, runRecorder)
import GridSelect (myColorizer, myGSConfig1) -- Gridselect module
import Variables (myTerminal, myAltTerminal, myLaunchManager, myTextEditor, myScreenshot, myScreenshotSelected)

-- ManageHooks
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks (ToggleStruts(ToggleStruts)) -- This module provides tools to automatically manage dock type programs, such as gnome-panel, kicker, dzen, and xmobar.
-- Key bindings
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioMute, xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_KbdBrightnessDown, xF86XK_KbdBrightnessUp, xF86XK_KbdLightOnOff) -- XF86 Extra keys https://xmonad.github.io/xmonad-docs/X11-1.10.3.9/Graphics-X11-ExtraTypes-XF86.html
import qualified Data.Map as M
import XMonad.Actions.CycleWS (moveTo, toggleWS', emptyWS, Direction1D(Next), WSType(Not), WSType(WSIs)) -- Provides bindings to cycle forward or backward through the list of workspaces, to move windows between workspaces, and to cycle between screens.
import XMonad.Actions.GridSelect (goToSelected)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotSlavesUp)                               -- Rotate all windows except the master window and keep the focus in place.
import XMonad.Actions.SinkAll (sinkAll)                                                  -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-SinkAll.html
import XMonad.Actions.OnScreen

-- Utils
import XMonad.Util.Run              (safeSpawn)                          --  https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Hooks-DynamicLog.htm


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  -- General
    [
      ((modm .|. shiftMask,   xK_c         ), kill                                                           ) -- Close focused window
    , ((modm,                 xK_space     ), sendMessage NextLayout                                         ) -- Rotate through the available layout algorithms
    , ((modm .|. shiftMask,   xK_space     ), setLayout $ XMonad.layoutHook conf                             ) -- Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask,   xK_n         ), refresh                                                        ) -- Resize viewed windows to the correct size
    , ((modm,                 xK_Tab       ), windows W.focusDown                                            ) -- Move focus to the next window
    , ((modm .|. shiftMask,   xK_Tab       ), windows W.focusUp                                              ) -- Move focus to the previous window
    , ((modm,                 xK_j         ), windows W.focusDown                                            ) -- Move focus to the next window
    , ((modm,                 xK_k         ), windows W.focusUp                                              ) -- Move focus to the previous window
    , ((modm,                 xK_m         ), windows W.focusMaster                                          ) -- Move focus to the master window
    , ((modm .|. shiftMask,   xK_m         ), windows W.swapMaster                                           ) -- Swap the focused window and the master window
    , ((modm .|. shiftMask,   xK_j         ), windows W.swapDown                                             ) -- Swap the focused window with the next window
    , ((modm .|. shiftMask,   xK_k         ), windows W.swapUp                                               ) -- Swap the focused window with the previous window
    , ((modm .|. controlMask, xK_j         ), rotSlavesDown                                                  ) -- Rotate all windows except the master window, while the focus stays where it is
    , ((modm .|. controlMask, xK_k         ), rotSlavesUp                                                    ) -- Rotate all windows except the master window, while the focus stays where it is
    , ((modm,                 xK_h         ), sendMessage Shrink                                             ) -- Shrink the master area
    , ((modm,                 xK_l         ), sendMessage Expand                                             ) -- Expand the master area
    , ((modm,                 xK_s         ), withFocused $ windows . W.sink                                 ) -- Push window back into tiling
    , ((modm .|. controlMask, xK_s         ), sinkAll                                                        ) -- Bring all float windows back to tile
    , ((modm,                 xK_b         ), sendMessage ToggleStruts                                       ) -- Toggle bar
    , ((modm,                 xK_comma     ), sendMessage (IncMasterN 1)                                     ) -- Increment the number of windows in the master area
    , ((modm,                 xK_period    ), sendMessage (IncMasterN (-1))                                  ) -- Deincrement the number of windows in the master area
    , ((modm,                 xK_Escape    ), toggleWS' ["NSP"]                                              ) -- Use Esc to toogle between workspaces with XMonad.CycleWS -- Ignore the scratchpad workspace while toggling
    , ((modm .|. controlMask, xK_grave     ), moveTo Next (Not emptyWS)                                      ) -- Move to the next non-empty workspace
    , ((modm .|. shiftMask,   xK_grave     ), moveTo Next (WSIs $ return (("NSP" /=) . W.tag))               ) -- Move to the next workspace which is not NSP
    , ((modm .|. shiftMask,   xK_apostrophe), io (exitWith ExitSuccess)                                      ) -- Quit xmonad
    , ((modm,                 xK_apostrophe), spawn "xmonad --recompile && xmonad --restart"                 ) -- Restart xmonad
    ]
    ++
    [
      ((0, xK_Print                        ), spawn myScreenshot                                             ) -- Print current display using maim with nametag: year-month-day-time-screenshot.png
    , ((modm, xK_Print                     ), spawn myScreenshotSelected                                     ) -- Xclip selected screen using maim
    , ((modm .|. shiftMask, xK_Print       ), runRecorder                                                    )
    , ((modm,                 xK_y         ), runRecorder                                                    ) -- Toggle bar
    , ((0, xF86XK_KbdBrightnessDown        ), spawn "brightnessctl set 20-"                                  ) -- F5 Monitor brightness down
    , ((0, xF86XK_KbdBrightnessUp          ), spawn "brightnessctl set +20"                                  ) -- F6 Monitor brightness up
    , ((0, xF86XK_KbdLightOnOff            ), spawn "~/.miozu/bin/backlight-toggle.sh"                       ) -- TODO F7 fix toggle monitor backlight
    , ((0, xF86XK_AudioMute                ), spawn "pactl set-sink-mute 0 toggle"                           ) -- F10 Mute
    , ((0, xF86XK_AudioLowerVolume         ), spawn "pactl set-sink-mute 0 false;pactl set-sink-volume 0 -5%") -- F11 Lower volume
    , ((0, xF86XK_AudioRaiseVolume         ), spawn "pactl set-sink-mute 0 false;pactl set-sink-volume 0 +5%") -- F12 Raise volume
    ]
    -- xF86XK media keybindings
    ++
    [-- Applications
      ((modm,                 xK_Return    ), spawn $ myTerminal                                             ) -- Launch a def terminal
    , ((modm .|. shiftMask,   xK_Return    ), spawn $ myAltTerminal                                          ) -- Launch a second terminal
    , ((modm,                 xK_m         ), spawn myLaunchManager                                          ) -- Launch rofii app launcher
    , ((modm .|. shiftMask,   xK_a         ), spawn "autorandr --change"                                     ) -- Launch Emacs TODO(add mods)
    , ((modm,                 xK_e         ), spawn myTextEditor                                             ) -- Launch Emacs TODO(add mods)
    , ((modm .|. shiftMask,   xK_d         ), spawn "dunstctl close-all"                                     ) -- Close all dunst notifications
    ]
    ++
    [-- ScratchPads keybindings
      ((modm,                 xK_r         ), scratchTermBR                                                  ) -- bir right terminal SP
    , ((modm .|. shiftMask,   xK_r         ), scratchTermSR                                                  ) -- small right terminal SP
    , ((modm,                 xK_t         ), scratchTermBL                                                  ) -- big left terminal SP
    , ((modm .|. shiftMask,   xK_t         ), scratchTermSL                                                  ) -- small right terminal SP
    , ((modm,                 xK_f         ), scratchFM                                                      ) -- file manager SP
    , ((modm .|. shiftMask,   xK_w         ), scratchWebA                                                    ) -- run chromium big SP
    , ((modm,                 xK_w         ), scratchWebB                                                    ) -- run chromium small SP
    ]
    ++
    -- [ -- SelectGrid
    -- TODO FIX BUG cause screen hang
    --   ((modm,                 xK_g         ), goToSelected $ myGSConfig1 myColorizer                         ) -- select workspace from grid
    -- ]
    -- ++
    -- mod-[1..9] [0], Switch to workspace N screen 0
    -- mod-shift-[1..9] [0], Move client to workspace N
    -- mod-ctl-[1..9] [0], screen 1
    -- mod-ctrl-shift-[1..9] [0], Move client to workspace N geedyView
    [ ((m .|. modm, k), windows (f i))
       | (i, k) <- zip (workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
       , (f, m) <-
         [ (viewOnScreen 0, 0)
         , (viewOnScreen 1, controlMask)
         , (W.shift, shiftMask)
         , (W.greedyView  , controlMask .|. shiftMask)
         ]
       ]
    ++
    -- changed default mod-{w,e,r} for dvorak mod-{comma,period,p}
    -- mod-{comma,period,p}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{comma,period,p}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_comma, xK_period, xK_p] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


-- TODO
--spawn $ "wezterm -e sh -c 'echo \"" ++ help ++ "\" | less'"

    -- [
    -- laptop specific
    --   ((0, xF86XK_KbdBrightnessDown        ), spawn "asus-kbd-backlight down"                                ) -- F3 Keyboard backlight down
    -- , ((0, xF86XK_KbdBrightnessUp          ), spawn "asus-kbd-backlight up"                                  ) -- F4 Keyboard backlight up
    -- ]
    -- ++
    -- [-- Audio control keybindings
    --  mcp is not in use anymare
    -- , ((0, xF86XK_AudioPlay                ), spawn "mpc toggle"                                             ) -- Arrow up
    -- , ((0, xF86XK_AudioStop                ), spawn "mpc stop"                                               ) -- Arrow down
    -- , ((0, xF86XK_AudioNext                ), spawn "mpc next"                                               ) -- Arrow right
    -- , ((0, xF86XK_AudioPrev                ), spawn "mpc prev"                                               ) -- Arrow left
    -- , ((modm, xK_Up                        ), spawn "mpc volume +20"                                         ) -- Arrow up volume
    -- , ((modm, xK_Down                      ), spawn "mpc volume -20"                                         ) -- Arrow down volume
    --]
    --, ((modm .|. controlMask, xK_g         ), gridselect myGSConfig2 myMpcGridList >>= flip whenJust spawn   ) -- select MPC command from grid
