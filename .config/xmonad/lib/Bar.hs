module Bar (mySB) where

import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers (logTitles)
import Variables (myBar)
import Miozu


------------------------------------------------------------------------
-- Status bar:
--
-- Perform an arbitrary action on each internal state change or X event.
-- This config will work with xmonad -v 0.17+
-- https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Hooks-StatusBar-PP.html
------------------------------------------------------------------------
mySB = statusBarProp myBar (pure myXmobarPP)

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = w "    •    " -- separator to use between different log sections
    , ppWsSep           = " "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = p . wrap " " "" . xmobarBorder "Top" peach 1
    , ppHidden          = gray . wrap " " ""
    , ppUrgent          = r . wrap (y " ") (y "")
    , ppOrder           = \[ws, _, t, _] -> [ws, t] -- workspace, layout, title, ...
    -- TODO find a way to apply alignment to %XMonadLog% output
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (b    "[") (b    "]") . b . ppWindow
    formatUnfocused = wrap (gray "[") (gray "]") . m . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    b, gray, m, r, w, y :: String -> [Char]
    m    = xmobarColor magenta ""
    b    = xmobarColor blue ""
    y    = xmobarColor yellow ""
    r    = xmobarColor red ""
    p    = xmobarColor peach ""
    w    = xmobarColor miozu04 ""
    gray = xmobarColor miozu02 ""
    dark = xmobarColor miozu00 ""
