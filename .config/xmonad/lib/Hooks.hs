module Hooks
  ( myEventHook
  , myStartupHook
  , LibNotifyUrgencyHook(..)
  ) where

import           XMonad
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
-- TODO write a log hook
---------------------------------------------------------------------------
-- logHook = composeAll [ fadeWindowsLogHook myFadeHook ]
-- , focusedBorderColor = myFocusedBorderColor
-- , normalBorderColor  = myNormalBorderColor
-- }
