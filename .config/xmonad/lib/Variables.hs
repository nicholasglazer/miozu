------------------------------------------------------------------------
-- Variables
------------------------------------------------------------------------
module Variables
  ( myFont,
    myBorderWidth,
    myModMask,
    myFocusedBorderColor,
    myNormalBorderColor,
    myFocusFollowsMouse,
    myClickJustFocuses,
    myTerminal,
    myAltTerminal,
    myTextEditor,
    myBar,
    myLaunchManager,
    myScrot,
    myScrotSelected,
    myByzanz,
  )
where

import Data.Time (formatTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import Miozu (miozu00, miozu02)
import XMonad (Dimension, KeyMask, mod4Mask)

myFont :: String
myFont = "xft:Hack:pixelsize=12"

myBorderWidth :: Dimension
myBorderWidth = 0

myModMask :: KeyMask
myModMask = mod4Mask

myFocusedBorderColor, myNormalBorderColor :: String
myFocusedBorderColor = miozu02
myNormalBorderColor = miozu00

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- The preferred programs, which is used in a binding below and by certain contrib modules.
myTerminal :: String
myTerminal = "/usr/bin/wezterm"

myAltTerminal :: String
myAltTerminal = "/usr/bin/wezterm"

myTextEditor :: String
myTextEditor = "emacsclient -nc"

myBar :: String
myBar = "/usr/bin/xmobar ~/.config/xmonad/xmobar/xmobar.hs"

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myLaunchManager :: String
myLaunchManager = "rofi -show drun -theme ~/.config/rofi/miozu.rasi"

myScrot :: String
myScrot = "scrot ~/Pictures/screenshots/%Y-%m-%d-%T-screenshot.png && notify-send 'Screenshot DONE'"

myScrotSelected :: String
myScrotSelected = "scrot -s ~/Pictures/screenshots/%Y-%m-%d-%T-screenshot.png"

myByzanz :: IO String
myByzanz =
  ("notify-send -t 2000 'Screen recording in:' '5 seconds' && sleep 5;notify-send -t 1500 Recording && byzanz-record -v -c --duration=15 -w 1920 -h 1080 ~/Pictures/gifs/screen_" ++)
    <$> fmap (formatTime defaultTimeLocale "%Y-%m-%d-%T_rec.gif  && notify-send 'Recorded and saved as:' 'screen_%Y-%m-%d-%T.gif'") getCurrentTime
