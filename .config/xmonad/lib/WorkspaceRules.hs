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
module WorkspaceRules (myManageHook) where

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook
import XMonad.Actions.OnScreen
import qualified XMonad.StackSet as W
import Workspaces (webWS, emacsWS, workWS, termWS, mediaWS, socialWS, toolsWS, magicWS, dualWS)
import Scratchpads (myScratchpads)

myManageHook = (composeAll . concat $
  [
    [ className =? c -->  doF (W.shift webWS   )                              | c <- myClassWebShifts     ]
  , [ className =? c -->  doF (W.shift emacsWS )                              | c <- myClassEmacsShifts   ]
  -- , [ className =? t --> (doF (W.view termWS   ) <+> doF (W.shift termWS   )) | t <- myClassTermShifts    ]
  -- , [ className =? t --> (doF (W.view workWS   ) <+> doF (W.shift workWS   )) | t <- myClassWorkShifts    ]
  -- , [ className =? t --> (doF (W.view mediaWS  ) <+> doF (W.shift mediaWS  )) | t <- myClassMediaShifts   ]
  -- , [ className =? t --> (doF (W.view socialWS ) <+> doF (W.shift socialWS )) | t <- myClassSocialShifts  ]
  -- , [ className =? t --> (doF (W.view toolsWS  ) <+> doF (W.shift toolsWS  )) | t <- myClassToolsShifts   ]
  -- , [ className =? t --> (doF (W.view magicWS  ) <+> doF (W.shift magicWS  )) | t <- myClassMagicShifts   ]
  -- , [ className =? t --> (doF (W.view dualWS   ) <+> doF (W.shift dualWS   )) | t <- myClassDualShifts    ]
  -- , [ className =? c --> doCenterFloat                                        | c <- myClassMagicShifts   ]
  -- , [ className =? c --> doFloat                                              | c <- myClassFloats        ]
  -- , [ className =? c --> doFullFloat                                          | c <- myClassFloats        ]
  --, [ isDialog       --> doCenterFloat                                                                    ]
  --, [ isFullscreen   --> doFullFloat                                                                      ]
  ]) <+> namedScratchpadManageHook myScratchpads
     <+> manageHook def

  where
    myClassWebShifts      = ["Firefox Beta"]
    myClassEmacsShifts    = ["Emacs"]
    -- myClassWorkShifts     = [""]
    -- myClassTermShifts     = ["Termite", "Konsole", "uxterm", "xterm", "", "org.wezfurlong.wezterm"]
    -- myClassMediaShifts    = ["vlc"]
    -- myClassSocialShifts   = ["TelegramDesktop", "discord"]
    -- myClassToolsShifts    = [""]
    -- myClassMagicShifts    = ["Wine", "Lutris"]
    -- myClassDualShifts     = [""]
    -- --myClassFloats         = ["firefox-developer-edition", "chromium", "FileManager"]
    -- myClassFloats         = [""]
    -- myClassFullFloats     = [""]
