import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Grid
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimpleFloat

import XMonad.Util.SpawnOnce


import qualified XMonad.StackSet as W
import Data.Bits ((.|.))
import qualified Data.Map as M

import System.Exit
import System.IO

_layout = avoidStruts (   Tall 1 (3/100) (1/2)
                      ||| Mirror (Tall 1 (3/100) (1/2))
                      ||| Full
                      ||| Grid
                      ||| spiral (6/7)
                      )
          |||
          simpleFloat

-- Keymappings are for Dvorak.
-- Original: http://xmonad.org/xmonad-docs/xmonad/src/XMonad-Config.html
_keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
_keys conf@(XConfig {XMonad.modMask = mod}) = M.fromList $
    -- Programs.
    [ ((mod, xK_o), spawn "chromium")
    , ((mod, xK_Up), spawn "amixer set Master playback 5%+")
    , ((mod, xK_Down), spawn "amixer set Master playback 5%-")

    -- , ((mod, xK_b), sendMessage ToggleStruts)

    -- Change keyboard mappings.
    -- , ((mod, xK_г), spawn "setxkbmap dvorak")
    -- , ((mod, xK_g), spawn "setxkbmap ru")

    -- Launching and killing programs.
    , ((mod, xK_Return), spawn $ XMonad.terminal conf)
    , ((mod, xK_r), spawn "dmenu_run")
    , ((mod.|. shiftMask, xK_c), kill)

    , ((mod, xK_space), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((mod.|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((mod, xK_b), refresh) -- %! Resize viewed windows to the correct size

    -- Move focus up or down the window stack.
    , ((mod, xK_Tab), windows W.focusDown) -- %! Move focus to the next window
    , ((mod.|. shiftMask, xK_Tab), windows W.focusUp) -- %! Move focus to the previous window
    , ((mod, xK_j), windows W.focusDown) -- %! Move focus to the next window
    , ((mod, xK_k), windows W.focusUp) -- %! Move focus to the previous window
    , ((mod, xK_m), windows W.focusMaster) -- %! Move focus to the master window

    -- Modifying the window order.
    , ((mod.|. shiftMask, xK_j), windows W.swapDown) -- %! Swap the focused window with the next window
    , ((mod.|. shiftMask, xK_k), windows W.swapUp) -- %! Swap the focused window with the previous window
    , ((mod.|. shiftMask, xK_m), windows W.swapMaster) -- %! Swap the focused window and the master window

    -- Resizing the master/slave ratio.
    , ((mod, xK_d), sendMessage Shrink) -- %! Shrink the master area
    , ((mod, xK_n), sendMessage Expand) -- %! Expand the master area

    -- Floating layer support.
    , ((mod, xK_y), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- Quit, or restart.
    , ((mod, xK_apostrophe), spawn "xmonad --recompile && xmonad --restart")
    , ((mod.|. shiftMask, xK_apostrophe), io exitSuccess)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. mod, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{,,.} %! Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{,,.} %! Move client to screen 1 or 2.
    [((m .|. mod, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_comma, xK_period] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

_config = defaultConfig { terminal = "termite"
                        , modMask = mod4Mask
                        -- , workspaces = myWorkspaces
                        , keys = _keys
                        , normalBorderColor  = "#333333"
                        , focusedBorderColor = "#5882FA"
                        , layoutHook = smartBorders _layout
                        , startupHook = spawnOnce "xmobar ~/.xmonad/xmobar.hs"
                        }


main = xmonad _config
