module Bar (mySB, myBottomSB) where

import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers (logTitles)
import Variables (myBar, myBottomBar)
import Miozu
import Workspaces (myWorkspaces)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)


------------------------------------------------------------------------
-- Status bar:
--
-- Perform an arbitrary action on each internal state change or X event.
-- This config will work with xmonad -v 0.17+
-- https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Hooks-StatusBar-PP.html
------------------------------------------------------------------------
mySB = statusBarProp myBar (pure myXmobarPP)

-- Bottom status bar for system monitoring (no XMonad log)
myBottomSB = statusBarProp myBottomBar (pure $ def { ppOutput = \_ -> return () })

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = w "    •    " -- separator between different log sections
    , ppWsSep           = " "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = p . wrap " " "" . xmobarBorder "Top" peach 1 . clickable
    , ppHidden          = gray . wrap " " "" . clickable
    , ppUrgent          = r . wrap (y " ") (y "") . clickable
    , ppOrder           = \[ws, _, t, _] -> [ws, t] -- workspace, layout, title, ...
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (b    "[") (b    "]") . b . ppWindow
    formatUnfocused = wrap (gray "[") (gray "]") . m . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    -- Make workspace clickable by wrapping with xmobar action tag
    clickable :: String -> String
    clickable ws = "<action=`/home/ng/.miozu/bin/xmonad-switch-workspace.sh " ++ wsIndex ++ "`>" ++ ws ++ "</action>"
      where
        wsIndex = show $ (+ 1) $ fromMaybe 0 $ elemIndex ws myWorkspaces

    b, gray, m, r, w, y :: String -> [Char]
    m    = xmobarColor magenta ""
    b    = xmobarColor blue ""
    y    = xmobarColor yellow ""
    r    = xmobarColor red ""
    p    = xmobarColor peach ""
    w    = xmobarColor miozu04 ""
    gray = xmobarColor miozu02 ""
    dark = xmobarColor miozu00 ""
