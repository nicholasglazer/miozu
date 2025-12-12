module Variables
  ( myFont
  , myBorderWidth
  , myModMask
  , myFocusedBorderColor
  , myNormalBorderColor
  , myFocusFollowsMouse
  , myClickJustFocuses
  , myTerminal
  , myAltTerminal
  , myTextEditor
  , myBar
  , myBottomBar
  , myLaunchManager
  , myDateFormat
  , myScreenshotsDir
  , myGifsDir
  , myScreenshot
  , myScreenshotSelected
  ) where

-- import           ScreenRecorder                 ( myGifRecorder )

import           Data.Time                      ( formatTime
                                                , getCurrentTime
                                                )
import           Data.Time.Format               ( defaultTimeLocale )
import           Miozu                          ( miozu00
                                                , miozu02
                                                )
import           System.Process                 ( readProcess )
import           XMonad                         ( Dimension
                                                , KeyMask
                                                , io
                                                , mod4Mask
                                                )


-- Behavior --
------------------------------------------------------------------------
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask :: KeyMask
myModMask = mod4Mask
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False
-- Default date formatting, mostly used in file names
myDateFormat :: String
myDateFormat = "$(date +'%s_%Y-%m-%d-%H_%M')"

-- Paths --
------------------------------------------------------------------------
-- Default screenshot dir
myScreenshotsDir :: String
myScreenshotsDir = "~/Pictures/screenshots/"
-- Default gif dir
myGifsDir :: String
myGifsDir = "~/Pictures/gifs/"
-- executes the fish shell to retrieve the value of the MIOZU_DIR variable
myMiozuDir :: IO String
myMiozuDir = readProcess "fish" ["-c", "echo $MIOZU_DIR"] ""

-- Pragrams --
------------------------------------------------------------------------
-- Call xmobar with its config file
myBar :: String
myBar = "xmobar ~/.config/xmonad/xmobar/xmobar.hs"
myBottomBar :: String
myBottomBar = "xmobar ~/.config/xmonad/xmobar/xmobar-bottom.hs"
-- Main terminal emulator
myTerminal :: String
myTerminal = "/usr/bin/alacritty"
-- Alt terminal fallback to xterm if foot fails
myAltTerminal :: String
myAltTerminal = "/usr/bin/xterm"
-- Run emacsclient if emacs daemon is running to avoid full emacs reload
myTextEditor :: String
myTextEditor = "emacsclient -nc"
-- Application launch manager using miozu theme
myLaunchManager :: String
myLaunchManager = "rofi -show drun -theme ~/.config/rofi/miozu.rasi"
-- Return the window ID of the window that the mouse cursor is currently over and make a screenshot
myScreenshot :: String
myScreenshot =
  "maim -i $(xdotool getmouselocation | grep -oP 'window:\\K[0-9a-fA-F]+') | tee "
    ++ myScreenshotsDir
    ++ myDateFormat
    ++ ".png | xclip -selection clipboard -t image/png && dunstify -h string:x-dunst-stack-tag:screenshot 'Screenshot saved and copied to clipboard'"
-- Make a screenshot of selected area also copy screenshot to a clipboard
myScreenshotSelected :: String
myScreenshotSelected =
  "maim -s | tee "
    ++ myScreenshotsDir
    ++ myDateFormat
    ++ ".png | xclip -selection clipboard -t image/png && dunstify -h string:x-dunst-stack-tag:screenshot 'Screenshot saved and copied to clipboard'"


-- Styles --
------------------------------------------------------------------------
myFont :: String
myFont = "xft:Inter:pixelsize=12"

myBorderWidth :: Dimension
myBorderWidth = 0

myFocusedBorderColor, myNormalBorderColor :: String
myFocusedBorderColor = miozu02
myNormalBorderColor = miozu00

