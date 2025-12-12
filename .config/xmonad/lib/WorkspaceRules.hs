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
module WorkspaceRules (myManageHook, role) where

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook
import XMonad.Actions.OnScreen
import qualified XMonad.StackSet as W
import Workspaces (webWS, emacsWS, workWS, termWS, mediaWS, socialWS, toolsWS, magicWS, dualWS)
import Scratchpads (myScratchpads)

-- Helper to match window role
role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

myManageHook = composeAll
  [
  -- Chromium: force tiling in workspace 2 as master, NEVER float
    (className =? "Chromium" <||> className =? "chromium" <||> className =? "Google-chrome") --> doF (W.shift emacsWS) <> doF W.swapMaster
  , (className =? "Chromium" <||> className =? "chromium") <&&> (role =? "browser") --> doF (W.shift emacsWS)
  ] <+> (composeAll . concat $
  [
    [ className =? c -->  doF (W.shift webWS   )                              | c <- myClassWebShifts     ]
  , [ className =? c -->  doF (W.shift emacsWS )                              | c <- myClassEmacsShifts   ]
  , [ className =? c -->  doF (W.shift toolsWS )                              | c <- myClassToolsShifts   ]
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
  ]) <+> namedScratchpadManageHook myScratchpads
     <+> manageHook def

  where
    myClassWebShifts      = ["Firefox Beta"]
    myClassEmacsShifts    = ["Emacs"]
    myClassToolsShifts    = ["Steam", "steam"]
    -- myClassWorkShifts     = [""]
    -- myClassTermShifts     = ["Termite", "Konsole", "uxterm", "xterm", "", "org.wezfurlong.wezterm"]
    -- myClassMediaShifts    = ["vlc"]
    -- myClassSocialShifts   = ["TelegramDesktop", "discord"]
    -- myClassMagicShifts    = ["Wine", "Lutris"]
    -- myClassDualShifts     = [""]
    -- --myClassFloats         = ["firefox-developer-edition", "chromium", "FileManager"]
    -- myClassFloats         = [""]
    -- myClassFullFloats     = [""]
