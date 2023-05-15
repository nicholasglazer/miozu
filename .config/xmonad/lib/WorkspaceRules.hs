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
module WorkspaceRules (
  myManageHook
) where

import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import Workspaces (webWS, emacsWS, workWS, termWS, mediaWS, socialWS, toolsWS, magicWS, privateWS)
import Scratchpads (myScratchpads)

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
     <+> namedScratchpadManageHook myScratchpads
     <+> manageHook def

  where
    myClassWebShifts      = ["Firefox", "Firefox Developer Edition"]
    myClassEmacsShifts    = ["Emacs"]
    myClassWorkShifts     = ["Chromium"]
    myClassTermShifts     = ["Termite","Konsole","uxterm","xterm"]
    myClassMediaShifts    = ["mpv", "vlc"]
    myClassSocialShifts   = ["TelegramDesktop"]
    myClassToolsShifts    = [""]
    myClassMagicShifts    = ["Wine", "Lutris"]
    myClassPrivateShifts  = ["Tor Browser"]
    myClassFloats         = ["Peek", "mpv"]
    myClassFullFloats     = [""]
