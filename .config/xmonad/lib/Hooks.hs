module Hooks
  ( myEventHook
  , myStartupHook
  , myLogHook
  , LibNotifyUrgencyHook(..)
  ) where

import           XMonad
import           Control.Monad                  ( when )
import           XMonad.Hooks.FadeWindows       ( fadeWindowsEventHook
                                                , isUnfocused
                                                , opacity
                                                , opaque
                                                )
import           XMonad.Hooks.UrgencyHook       ( UrgencyHook(..) )
import qualified XMonad.StackSet               as W
import           XMonad.Util.NamedWindows       ( getName )
import           XMonad.Util.Run                ( hPutStrLn
                                                , safeSpawn
                                                )
import           XMonad.Util.SpawnOnce          ( spawnOnce )
import           XMonad.Hooks.SetWMName         ( setWMName )
import qualified XMonad.StackSet               as W
import           Workspaces                     ( toolsWS )
--import XMonad.Hooks.FadeWindows (fadeWindowsLogHook)

------------------------------------------------------------------------
-- Startup hook:
--
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  spawn "bash $MIOZU_DIR/bin/display/device_mono.sh"
  spawn "setxkbmap -layout \"us,ua\" -variant \"dvorak,\" -option \"grp:alt_shift_toggle,caps:escape\""
  spawnOnce "emacsclient -nc --eval '(doom/quickload-session)'"
  setWMName "LG3D"


------------------------------------------------------------------------
-- Event handling:
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
------------------------------------------------------------------------
myEventHook = fadeWindowsEventHook

---------------------------------------------------------------------------
-- Urgency Hook:
--
-- Allows you to use notifications for xmonad
-- thanks to https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
---------------------------------------------------------------------------
data LibNotifyUrgencyHook = LibNotifyUrgencyHook
  deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- fmap (W.findTag w) $ gets windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]


------------------------------------------------------------------------
-- Fade Windows:
--
-- You will need picom to be installed to use this
-- you can add [ transparency 0.1 ] to all windows where 1 is fully transparent (use with caution)
-- TODO test this, since compton was replaced by steamos-compositor-plus
------------------------------------------------------------------------
-- myFadeHook =
--   composeAll
--     [ opaque,
--       isUnfocused --> opacity 1,
--       (className =? "wezterm") <&&> (isUnfocused) --> opacity 0.55
--     ]

---------------------------------------------------------------------------
-- Log Hook:
--
-- Handle workspace switching events and set appropriate keyboard layout
---------------------------------------------------------------------------
myLogHook :: X ()
myLogHook = return ()
-- Workspace-based keyboard switching disabled to prevent leader key issues
-- Use manual keyboard switching with Alt+Shift instead
