--   INFO
--  ------
-- information = { Author   = Nicholas Glazer
--               , Version  = xmonad 0.15 <+> ghc 8.6.1
--               , Updated  = November 23 2018
--               }
--
-- This config based on xmonad example config file and hackage.haskell.org documentation - https://github.com/xmonad/xmonad/blob/master/man/xmonad.hs
--
-- You also can find some useful constructions here: https://www.nepherte.be/blog/step-by-step-configuration-of-xmonad.html
--

{-# LANGUAGE NoMonomorphismRestriction #-}

--- IMPORTS
import XMonad
import System.Exit                  (exitWith, ExitCode(ExitSuccess))

import Data.Time                    (formatTime, getCurrentTime)                               -- Time library
import Data.Time.Format             (defaultTimeLocale)


-- ManageHooks
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks     (ToggleStruts(ToggleStruts), docks, avoidStruts, manageDocks, docksEventHook) -- This module provides tools to automatically manage dock type programs, such as gnome-panel, kicker, dzen, and xmobar.
import XMonad.Hooks.EwmhDesktops    (ewmh, ewmhDesktopsEventHook, ewmhDesktopsLogHook)         -- Tell panel applications about its workspaces and the windows therein.
import XMonad.Hooks.ManageHelpers   (isDialog, isFullscreen, doCenterFloat, doFullFloat)       -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-ManageHelpers.html
import XMonad.Util.NamedScratchpad                                                             -- https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Util-NamedScratchpad.html
import XMonad.Util.NamedWindows     (getName)                                                  -- This module allows you to associate the X titles of windows with them.


-- LogHook
import XMonad.Hooks.DynamicLog                                                                 -- Bar log hook
import XMonad.Hooks.FadeWindows                                                                -- Flexible and general compositing interface than FadeInactive
import XMonad.Hooks.UrgencyHook     (urgencyHook, UrgencyHook, withUrgencyHook)                -- Workspaces containing windows that require your attention will be highlighted.

-- Window Layout Mode
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen     (fullscreenFull, fullscreenManageHook, fullscreenSupport)  -- Provides a ManageHook and an EventHook that sends layout messages with information about fullscreening windows.
import XMonad.Layout.WindowArranger (windowArrange)                                            -- Arrange windows with mouse/touchpad
import XMonad.Layout.NoBorders      (noBorders, smartBorders)                                  -- Borders manipulations
import XMonad.Layout.Accordion      (Accordion(Accordion))
import XMonad.Layout.Circle         (Circle(Circle))                                           -- A layout with floating windows https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Layout-Circle.html
import XMonad.Layout.Grid           (Grid(Grid))
import XMonad.Layout.ThreeColumns   (ThreeCol(ThreeColMid))                                    -- Layout with three columns
import XMonad.Layout.PerWorkspace   (onWorkspace)                                              -- http://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Layout-PerWorkspace.html

-- Key bindings
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86                                                            -- XF86 Extra keys http://xmonad.org/xmonad-docs/X11/Graphics-X11-ExtraTypes-XF86.html
import XMonad.Actions.CycleWS       (moveTo, toggleWS', Direction1D(Next), WSType(WSIs, NonEmptyWS)) -- Provides bindings to cycle forward or backward through the list of workspaces, to move windows between workspaces, and to cycle between screens.
import XMonad.Actions.DynamicProjects                                                          -- Imbues workspaces with additional features so they can be treated as individual project areas.
import XMonad.Actions.GridSelect                                                               -- Displays items(e.g. the opened windows) in a 2D grid and lets the user select from it with the cursor/hjkl keys or the mouse.
import XMonad.Actions.RotSlaves     (rotSlavesDown, rotSlavesUp)                               -- Rotate all windows except the master window and keep the focus in place.
import XMonad.Actions.SinkAll       (sinkAll)                                                  -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-SinkAll.html

-- Utils
import XMonad.Util.SpawnOnce        (spawnOnce)                                                -- A module for spawning a command once, and only once.
import XMonad.Util.Run              (spawnPipe, hPutStrLn, safeSpawn)                          --  https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Hooks-DynamicLog.htm



-------------------------------------------------------------------------------
-- Colors - monokai like palette
-------------------------------------------------------------------------------
--
black      = "#272822"
grayD      = "#3e3d31"
red        = "#e03a3a"
maroon     = "#f92672"
green      = "#3ae03a"
greenL     = "#a6e22e"
blue       = "#63b8ff"
blueD      = "#1e90ff"
orange     = "#fd971f"
yellow     = "#e6db74"
magenta    = "#fd5ff0"
violet     = "#ae81ff"
cyan       = "#a1efe4"
cyanD      = "#3ae0e0"
gray       = "#64645e"
white      = "#f9f9f2"
textColor  = "#fffacd"
textColorA = "#ffdead"

------------------------------------------------------------------------
-- Variables
------------------------------------------------------------------------
myFont :: String
myFont = "xft:Hack:pixelsize=15"

myBorderWidth :: Dimension
myBorderWidth = 4

myModMask :: KeyMask
myModMask = mod4Mask

myFocusedBorderColor, myNormalBorderColor :: String
myFocusedBorderColor = orange
myNormalBorderColor = grayD

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- The preferred programs, which is used in a binding below and by certain contrib modules.
myTerminal :: String
myTerminal = "/usr/bin/termite"

myAltTerminal :: String
myAltTerminal = "/usr/bin/urxvt"

myTextEditor :: String
myTextEditor = "/usr/bin/emacs"

myBar :: String
myBar = "/usr/bin/xmobar ~/.config/xmobar/xmobarrc"
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myLaunchManager :: String
myLaunchManager = "rofi -combi-modi drun,ssh -theme onemon -font 'hack 12' -show combi" -- SelectGrid settings

myScrot :: String
myScrot = "scrot ~/images/screenshots/%Y-%m-%d-%T-screenshot.png && notify-send 'Screenshot DONE'"

myByzanz :: IO String
myByzanz = ("notify-send -t 2000 'Screen recording in:' '5 seconds' && sleep 5;notify-send -t 1500 Recording && byzanz-record -v -d 5 -w 1920 -h 1080 ~/images/gif/screen_" ++)
     <$> fmap (formatTime defaultTimeLocale "%Y-%m-%d-%T_rec.gif  && notify-send 'Recorded and saved as:' 'screen_%Y-%m-%d-%T.gif'") getCurrentTime

-------------------------------------------------------------------------------
-- GridSelect config
-------------------------------------------------------------------------------
myMpcGridList = [
    ("Toggle"      , "mpc toggle"                                         )
  , ("Update"      , "mpc update /"                                       )
  , ("Next"        , "mpc next"                                           )
  , ("Start random", "mpc clear && mpc add / && mpc random on && mpc play")
  , ("Prev"        , "mpc prev"                                           )
  ]

myColorizer = colorRangeFromClassName
                     (0x27,0x28,0x22) -- lowest inactive bg
                     (0x27,0x28,0x22) -- highest inactive bg
                     (0x3e,0x3d,0x31) -- active bg
                     (0xff,0xfa,0xcd) -- inactive fg
                     (0xf9,0xf9,0xf2) -- active fg

myGSConfig1 colorizer = (buildDefaultGSConfig colorizer)
    { gs_cellheight   = 80
    , gs_cellwidth    = 200
    , gs_cellpadding  = 8
    , gs_bordercolor = orange
    }

myGSConfig2 = def
    { gs_cellheight   = 80
    , gs_cellwidth    = 200
    , gs_cellpadding  = 8
    , gs_bordercolor  = orange
    , gs_font         = myFont
    }
------------------------------------------------------------------------
-- Workspaces
--
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
-- Sending Font Awesome gliphs to xmobar.
------------------------------------------------------------------------
webWS     = " <fn=4>\xf55f</fn> "
emacsWS   = " <fn=4>\xf679</fn> "
termWS    = " <fn=4>\xf120</fn> "
workWS    = " <fn=4>\xf54c</fn> "
mediaWS   = " <fn=4>\xf630</fn> "
socialWS  = " <fn=4>\xf075</fn> "
toolsWS   = " <fn=4>\xf568</fn> "
magicWS   = " <fn=4>\xf6e8</fn> "
privateWS = " <fn=4>\xf6fa</fn> "

myWorkspaces :: [String]
myWorkspaces =  [webWS, emacsWS, termWS, workWS, mediaWS, socialWS, toolsWS, magicWS, privateWS]
------------------------------------------------------------------------
-- Layouts:
--
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts. Note that each layout is separated by |||,
-- which denotes layout choice.
------------------------------------------------------------------------
mediaLayout = noBorders $ fullscreenFull Full ||| Circle                                             -- Media layout with ability to change full screen

workLayout =  tiled ||| Full ||| gap ||| gapM ||| noBorders Accordion
    where
      nmaster = 1                                                                                    -- The default number of windows in the master pane
      ratio   = 0.5                                                                                  -- Default proportion of screen occupied by master pane
      delta   = 0.3                                                                                  -- Percent of screen to increment by when resizing panes
      tiled   = Tall nmaster delta ratio                                                             -- tiling default tall column
      gapM    = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True
                $ Mirror $ Tall nmaster delta ratio                                                  -- tiling algo partitions the screen with spacing mirrored tall
      gap     = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True
                $ Tall nmaster delta ratio                                                           -- tiling algo partitions the screen with spacing tall

defLayout =  gap ||| noBorders Full ||| Circle ||| Grid ||| noBorders Accordion
    where
      nmaster = 1                                                                                    -- The default number of windows in the master pane
      ratio   = 0.5                                                                                  -- Default proportion of screen occupied by master pane
      delta   = 0.3                                                                                  -- Percent of screen to increment by when resizing panes
      gap     = spacingRaw True (Border 10 20 20 20) True (Border 20 20 20 20) True
                $ ThreeColMid nmaster delta ratio                                                    -- tiling algo partitions the screen into three columns with spacing

myLayout = avoidStruts . smartBorders $ windowArrange
           $ onWorkspace emacsWS workLayout
           $ onWorkspace mediaWS mediaLayout
           $ onWorkspace workWS workLayout
           $ defLayout
------------------------------------------------------------------------
-- ScratchPads:
--
-- Very usable and convenient tool for quick programs launch and then hide them with the same keybinding (toggle).
-- more info in article https://pbrisbin.com/posts/scratchpad_everything/
------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads =  [ NS "terminalS" spawnTermS  findTermS  manageTermS    -- Small terminal scratchpad
                 , NS "terminalB" spawnTermB  findTermB  manageTermB    -- Big terminal scratchpad
                 , NS "mpd"       spawnMpd    findMpd    manageMpd      -- Mpd scratchpad
                  -- , NS "mpv"       spawnMpv    findMpv    manageMpv      -- Mpv scratchpad TODO
                 , NS "mixer"     spawnMixer  findMixer  manageMixer    -- Mixer scratchpad
                 , NS "ranger"    spawnRanger findRanger manageRanger   -- Ranger scratchpad
                 ]
 where
    role = stringProperty "WM_WINDOW_ROLE"

    spawnTermS   = "urxvt -name Scratchpad:Terminal_Bottom"             -- launch terminal
    findTermS    = resource  =? "Scratchpad:Terminal_Bottom"            -- its window will be named "Scratchpad:Terminal_Bottom"
    manageTermS  = customFloating $ W.RationalRect l t w h              -- and the geometry:
      where
        h = 0.15      -- height 15%
        w = 0.98      -- width 98%
        t = 0.98 - h  -- top 98%
        l = (1 - w)/2 -- centered left/right

    spawnTermB   = "urxvt -name Scratchpad:Terminal_Big"                -- launch terminal
    findTermB    = resource  =? "Scratchpad:Terminal_Big"               -- its window will be named "Scratchpad:Terminal_Big"
    manageTermB  = customFloating $ W.RationalRect l t w h              -- and the geometry:
      where
        h = 0.78      -- height 78%
        w = 0.57      -- width 57%
        t = 0.03      -- top 3%
        l = 0.99 - w  -- left 99%

    spawnMixer   = "urxvt -name Pulsemixer -e pulsemixer"              -- launch pulsemixer
    findMixer    = resource =? "Pulsemixer"                            -- its window will be named "pulsemixer"
    manageMixer  = customFloating $ W.RationalRect l t w h             -- and the geometry:
      where
        h = 0.1       -- height 10%
        w = 0.40      -- width  40%
        t = 0.03      -- top 3%
        l = 0.01      -- left 1%

    spawnMpd     = "urxvt -name MPD -e pms"                            -- launch pmus-git
    findMpd      = resource  =? "MPD"                                  -- its window will be named "mpd"
    manageMpd    = customFloating $ W.RationalRect l t w h             -- and the geometry:
      where
        h = 0.66      -- height, 66%
        w = 0.40      -- width,  40%
        t = 0.15      -- top 15%
        l = 0.01      -- left 1%

    spawnRanger  = "urxvt -name Ranger -e ranger"                      -- launch ranger
    findRanger   = resource =? "Ranger"                                -- its window will be named "ranger"
    manageRanger = customFloating $ W.RationalRect l t w h             -- and the geometry:
      where
        h = 0.78      -- height 78%
        w = 0.98      -- width 98%
        t = 0.03      -- top 3%
        l = (1 - w)/2 -- centered left/right

------------------------------------------------------------------------
-- Projects:
--
-- DynamicProjects allows you to dedicate workspaces to specific projects and then switch between projects easily.
-- A project is made up of a name, working directory, and a start-up hook.
-- When you switch to a workspace, DynamicProjects changes the working directory to the one configured for the matching project.
-- If the workspace doesn't have any windows, the project's start-up hook is executed.
-- This allows you to launch applications or further configure the workspace/project.
------------------------------------------------------------------------
projects :: [Project]
projects =
  [ Project { projectName      = "scratch"
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }

  , Project { projectName      = emacsWS
            , projectDirectory = "~/code"
            , projectStartHook = Just $ do spawn myTextEditor
            }

  , Project { projectName      = termWS
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn myTerminal
            }

  , Project { projectName      = privateWS
            , projectDirectory = "~/tor"
            , projectStartHook = Just $ do spawnOnce "~/tor/start-tor-browser.desktop"
            }
  ]
------------------------------------------------------------------------
-- Window rules:
--
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
------------------------------------------------------------------------
myManageHook = (composeAll . concat $
  [
    [ className =? c -->  doF (W.shift webWS   )                              | c <- myClassWebShifts     ]
  , [ className =? c -->  doF (W.shift emacsWS )                              | c <- myClassEmacsShifts   ]
  , [ className =? t --> (doF (W.view termWS   ) <+> doF (W.shift termWS   )) | t <- myClassTermShifts    ]
  , [ className =? t --> (doF (W.view workWS   ) <+> doF (W.shift workWS   )) | t <- myClassWorkShifts    ]
  , [ className =? t --> (doF (W.view mediaWS  ) <+> doF (W.shift mediaWS  )) | t <- myClassMediaShifts   ]
  , [ className =? t --> (doF (W.view socialWS ) <+> doF (W.shift socialWS )) | t <- myClassSocialShifts  ]
  , [ className =? t --> (doF (W.view toolsWS  ) <+> doF (W.shift toolsWS  )) | t <- myClassToolsShifts   ]
  , [ className =? t --> (doF (W.view magicWS  ) <+> doF (W.shift magicWS  )) | t <- myClassMagicShifts   ]
  , [ className =? t --> (doF (W.view privateWS) <+> doF (W.shift privateWS)) | t <- myClassPrivateShifts ]
  , [ className =? c --> doCenterFloat                                        | c <- myClassMagicShifts   ]
  , [ className =? c --> doFloat                                              | c <- myClassFloats        ]
  , [ className =? c --> doFullFloat                                          | c <- myClassFloats        ]
  , [ isDialog       --> doCenterFloat                                                                    ]
  , [ isFullscreen   --> doFullFloat                                                                      ]
  ]) <+> manageDocks
     <+> fullscreenManageHook
     <+> namedScratchpadManageHook myScratchPads
     <+> manageHook def

  where
    myClassWebShifts      = ["Vivaldi-stable", "Chromium"]
    myClassEmacsShifts    = ["Emacs"]
    myClassTermShifts     = ["Termite","Konsole","uxterm","xterm"]
    myClassWorkShifts     = ["Firefox Developer Edition"]
    myClassMediaShifts    = ["mpv", "vlc"]
    myClassSocialShifts   = ["TelegramDesktop"]
    myClassToolsShifts    = ["GParted", "transmission"]
    myClassMagicShifts    = ["Wine"]
    myClassPrivateShifts  = ["Tor Browser"]
    myClassFloats         = ["Peek", "mpv"]
    myClassFullFloats     = ["deepin-image-viewer"]

------------------------------------------------------------------------
-- Fade Windows:
--
-- You will need compton to be installed to use this
-- you can add [ transparency 0.1 ] to all windows - 1 is fully transparent(be aware of using it;)
------------------------------------------------------------------------
myFadeHook = composeAll [  opaque
                         , isUnfocused --> opacity 1
                         , (className =? "URxvt") <&&> (isUnfocused) --> opacity 1
                         , (className =? "Termite") <&&> (isUnfocused) --> opacity 0.55
                         ]

------------------------------------------------------------------------
-- Event handling:
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
------------------------------------------------------------------------
myEventHook = mconcat [
    docksEventHook
  , fadeWindowsEventHook
  , ewmhDesktopsEventHook
  ]

------------------------------------------------------------------------
-- Startup hook:
--
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
------------------------------------------------------------------------
myStartupHook :: X()
myStartupHook = do
    -- xsetroot -cursor_name left_ptr &
    spawnOnce "xrdb -merge ~/.Xresources"                                                   -- Read .Xresources
    spawnOnce "setxkbmap -layout 'dvorak, ru' -option 'grp:alt_shift_toggle, caps: escape'" -- Set dv keyboard layout, Caps Lock serve as an ESC
    spawnOnce "$HOME/.fehbg"                                                                -- Read feh config for background
    spawnOnce "sleep 1;mpd"                                                                 -- Launch mpd
    spawnOnce "sleep 2;setWMName 'LG3D'"                                                    -- For same programs to work properly
    spawnOnce "sleep 3;dex /etc/xdg/autostart/polkit-kde-authentication-agent-1.desktop"    -- Run polkit agent

---------------------------------------------------------------------------
-- Urgency Hook:
--
-- Allows you to use notifications for xmonad
-- thanks to https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
---------------------------------------------------------------------------
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

------------------------------------------------------------------------
-- Status bar:
--
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
-- https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Hooks-DynamicLog.html
--
-- Leave it here in case I'll need to see all hidden workspaces without windows ppHiddenNoWindows
--
-- where
-- showWsNames wsId = if any (`elem` wsId) ['a'..'z']
--                                      then pad wsId
--                                      else ""
--
-- Pretty printer for xmobar:
------------------------------------------------------------------------
myLogHook xmprocs =  dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP
  $ xmobarPP {
      ppOutput          = hPutStrLn xmprocs
    , ppCurrent         = xmobarColor greenL ""
    , ppHidden          = xmobarColor orange ""
    , ppTitle           = xmobarColor gray "" . shorten 80
    , ppUrgent          = xmobarColor maroon ""
    , ppLayout          = xmobarColor blue ""
    , ppOrder           = \(ws:l:t:_) -> [ws,l,t]                  -- workspace, layout, title
    , ppSep             = "  |  "                                  -- separator to use between different log sections
    , ppWsSep           = " "
    -- , ppHiddenNoWindows = showWsNames                           -- To show all hidden workspaces
    -- , ppVisibleNoWindows = Nothing                              -- To define how should look visible workspaces without windows
}

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
-- Run in termilan to find needed key:
-- xev | fgrep "keysym"
------------------------------------------------------------------------
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  -- General
    [
      ((modm .|. shiftMask,   xK_c         ), kill                                                           ) -- close focused window
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
    , ((modm,                 xK_grave     ), toggleWS' ["NSP"]                                              ) -- Use tilde to toogle between workspaces with XMonad.CycleWS -- Ignore the scratchpad workspace while toggling
    , ((modm .|. controlMask, xK_grave     ), moveTo Next NonEmptyWS                                         ) -- Move to the next non-empty workspace
    , ((modm .|. shiftMask,   xK_grave     ), moveTo Next (WSIs $ return (("NSP" /=) . W.tag))               ) -- Move to the next workspace which is not NSP
    , ((modm .|. shiftMask,   xK_apostrophe), io (exitWith ExitSuccess)                                      ) -- Quit xmonad
    , ((modm,                 xK_apostrophe), spawn "xmonad --recompile && xmonad --restart"                 ) -- Restart xmonad
    , ((modm .|. shiftMask,   xK_slash     ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -")           ) -- Run xmessage with a summary of the default keybindings (useful for beginners)
    ]
    ++
    [ -- xF86XK media keybindings
      ((0, xF86XK_KbdBrightnessDown        ), spawn "asus-kbd-backlight down"                                ) -- F3 Keyboard backlight down
    , ((0, xF86XK_KbdBrightnessUp          ), spawn "asus-kbd-backlight up"                                  ) -- F4 Keyboard backlight up
    , ((0, xF86XK_MonBrightnessDown        ), spawn "xbacklight -dec 10%"                                    ) -- F5 Monitor brightness down
    , ((0, xF86XK_MonBrightnessUp          ), spawn "xbacklight -inc 10%"                                    ) -- F6 Monitor brightness up
    -- , ((0, xF86XK_MonBrightness       ),   spawn  "~/.dotfiles/bin/xbacklight-toggle.sh"                 ) -- TODO F7 fix toggle monitor backlight
    , ((0, xF86XK_AudioMute                ), spawn "pactl set-sink-mute 0 toggle"                           ) -- F10 Mute
    , ((0, xF86XK_AudioLowerVolume         ), spawn "pactl set-sink-mute 0 false;pactl set-sink-volume 0 -5%") -- F11 Lower volume
    , ((0, xF86XK_AudioRaiseVolume         ), spawn "pactl set-sink-mute 0 false;pactl set-sink-volume 0 +5%") -- F12 Raise volume
    , ((0, xK_Print                        ), spawn myScrot                                                  ) -- Print screen using scrot with nametag: year-month-day-time-screenshot.png
    , ((modm, xK_Print                     ), liftIO myByzanz >>= spawn                                      ) -- Print screen using scrot with nametag: year-month-day-time-screenshot.png
    ]
    ++
    [-- Audio control keybindings
      ((modm, xK_Up                        ), spawn "mpc volume +20"                                         ) -- Arrow up volume
    , ((modm, xK_Down                      ), spawn "mpc volume -20"                                         ) -- Arrow down volume
    , ((0, xF86XK_AudioPlay                ), spawn "mpc toggle"                                             ) -- Arrow up
    , ((0, xF86XK_AudioStop                ), spawn "mpc stop"                                               ) -- Arrow down
    , ((0, xF86XK_AudioNext                ), spawn "mpc next"                                               ) -- Arrow right
    , ((0, xF86XK_AudioPrev                ), spawn "mpc prev"                                               ) -- Arrow left
    ]
    ++
    [-- Programs
      ((modm,                 xK_Return    ), spawn $ myTerminal                                             ) -- Launch a def terminal
    , ((modm .|. shiftMask,   xK_Return    ), spawn $ myAltTerminal                                          ) -- Launch a second terminal
    , ((modm,                 xK_d         ), spawn myLaunchManager                                          ) -- Launch Rofi app launcher
    , ((modm,                 xK_n         ), spawn "networkmanager_dmenu"                                   ) -- Launch NetworkManager applet
    , ((modm,                 xK_e         ), spawn myTextEditor                                             ) -- Launch Emacs TODO(add mods)
    , ((modm,                 xK_v         ), spawn "vivaldi-stable"                                         ) -- Launch Vivaldi browser
    ]
    ++
    [-- ScratchPads keybindings
      ((modm,                 xK_t         ), scratchTermS                                                   ) -- small terminal SP
    , ((modm .|. shiftMask,   xK_t         ), scratchTermB                                                   ) -- big terminal SP
    , ((modm,                 xK_g         ), scratchMusic                                                   ) -- music player pms SP
    , ((modm .|. shiftMask,   xK_g         ), scratchMixer                                                   ) -- pulsemixer SP
    , ((modm,                 xK_r         ), scratchRanger                                                  ) -- file manager SP
    ]
    ++
    [-- SelectGrid
      ((modm,                 xK_w         ), goToSelected $ myGSConfig1 myColorizer                         ) -- select workspace from grid
    , ((modm .|. controlMask, xK_g         ), gridselect myGSConfig2 myMpcGridList >>= flip whenJust spawn   ) -- select MPC command from grid
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- changed default mod-{w,e,r} for dvorak mod-{comma,period,p}
    -- mod-{comma,period,p}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{comma,period,p}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_comma, xK_period, xK_p] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    where
      -- Define scratchsads
      scratchTermS   = namedScratchpadAction myScratchPads "terminalS"
      scratchTermB   = namedScratchpadAction myScratchPads "terminalB"
      scratchMixer   = namedScratchpadAction myScratchPads "mixer"
      scratchMusic   = namedScratchpadAction myScratchPads "mpd"
      scratchRanger  = namedScratchpadAction myScratchPads "ranger"

------------------------------------------------------------------------
-- Main:
--
-- Run xmonad with all specified settings.
------------------------------------------------------------------------
main = do
  xmprocs <- spawnPipe myBar
  xmonad
    $ withUrgencyHook LibNotifyUrgencyHook
    $ dynamicProjects projects
    $ fullscreenSupport
    $ docks
    $ ewmh defaults {
    logHook = composeAll [
        fadeWindowsLogHook myFadeHook
        , ewmhDesktopsLogHook
        , myLogHook xmprocs
        ]
    , focusedBorderColor = myFocusedBorderColor
    , normalBorderColor  = myNormalBorderColor
    }

-- No need to modify defaults.
defaults = def {
  -- simple stuff
  terminal           = myTerminal,
  focusFollowsMouse  = myFocusFollowsMouse,
  clickJustFocuses   = myClickJustFocuses,
  borderWidth        = myBorderWidth,
  modMask            = myModMask,
  workspaces         = myWorkspaces,
  -- key bindings
  keys               = myKeys,
  -- hooks, layouts
  layoutHook         = myLayout,
  manageHook         = myManageHook,
  handleEventHook    = myEventHook,
  startupHook        = myStartupHook
  }


------------------------------------------------------------------------
-- TODO add all keybindings into help
-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'mod4Mask' - windows key. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Enter        Launch Termite",
    "mod-Shift-Enter  Launch URxvt",
    "mod-d            Launch application launcher Rofi",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-Shift-n      Resize/refresh viewed windows to the correct size",
    "mod-n            Launch NetworkManager applet",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-apostrophe  Quit xmonad",
    "mod-apostrophe        Restart xmonad",
    "",
    "-- Workspaces & screens",
    "mod-[1..9]                  Switch to workSpace N",
    "mod-Shift-[1..9]            Move client to workspace N",
    "mod-mod-{comma,period,p}    witch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{comma,period,p}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
