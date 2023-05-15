module Hooks
  ( myFadeHook,
    myEventHook,
    myStartupHook,
    LibNotifyUrgencyHook (..),
    myLogHook,
  )
where

import Miozu (miozu02, miozu03, peach, red)
import XMonad
import XMonad.Hooks.DynamicLog (PP (ppCurrent, ppHidden, ppLayout, ppOrder, ppOutput, ppSep, ppTitle, ppUrgent, ppWsSep), dynamicLogWithPP, shorten, xmobarColor, xmobarPP)
import XMonad.Hooks.FadeWindows (fadeWindowsEventHook, isUnfocused, opacity, opaque)
import XMonad.Hooks.UrgencyHook (UrgencyHook (..))
import XMonad.ManageHook (className)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn, hPutStrLn)
import XMonad.Util.SpawnOnce (spawnOnce)

------------------------------------------------------------------------
-- Fade Windows:
--
-- You will need compton to be installed to use this
-- you can add [ transparency 0.1 ] to all windows where 1 is fully transparent (use with caution)
-- TODO test this, since compton was replaced by steamos-compositor-plus
------------------------------------------------------------------------
myFadeHook =
  composeAll
    [ opaque,
      isUnfocused --> opacity 1,
      (className =? "wezterm") <&&> (isUnfocused) --> opacity 0.55
    ]

------------------------------------------------------------------------
-- Event handling:
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
------------------------------------------------------------------------
myEventHook = fadeWindowsEventHook

------------------------------------------------------------------------
-- Startup hook:
--
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  -- spawnOnce "feh --no-fehbg --bg-scale '$HOME/.config/wallpapers/miozu.png'"

  spawnOnce "emacsclient -nc --eval '(doom/quickload-session)'"

---------------------------------------------------------------------------
-- Urgency Hook:
--
-- Allows you to use notifications for xmonad
-- thanks to https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
---------------------------------------------------------------------------
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    Just idx <- fmap (W.findTag w) $ gets windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]

------------------------------------------------------------------------
-- Status bar:
--
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Hooks-DynamicLog.html
------------------------------------------------------------------------
myLogHook xmprocs =
  dynamicLogWithPP $
    xmobarPP
      { ppOutput = hPutStrLn xmprocs,
        ppCurrent = xmobarColor peach "",
        ppHidden = xmobarColor miozu02 "",
        ppTitle = xmobarColor miozu03 "" . shorten 80,
        ppUrgent = xmobarColor red "",
        ppLayout = xmobarColor miozu02 "",
        ppOrder = \(ws : _ : t : _) -> [ws, t], -- workspace, layout, title
        ppSep = "    ", -- separator to use between different log sections
        ppWsSep = " "
        -- , ppHiddenNoWindows = showWsNames                           -- To show all hidden workspaces
        -- , ppVisibleNoWindows = Nothing                              -- To define how should look visible workspaces without windows
      }
