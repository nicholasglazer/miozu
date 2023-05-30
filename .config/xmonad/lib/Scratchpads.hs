------------------------------------------------------------------------
-- ScratchPads:
--
-- Useful and convenient tool to quickly launch programs in predictible
-- places above all windows, hide them after using same keybinding.
-- more info in article https://pbrisbin.com/posts/scratchpad_everything/
------------------------------------------------------------------------
module Scratchpads
  ( myScratchpads
  , scratchTermSL
  , scratchTermBL
  , scratchTermSR
  , scratchTermBR
  , scratchFM
  , scratchWebA
  , scratchWebB
  ) where

import           Variables                      ( myTerminal )
import           XMonad
import qualified XMonad.StackSet               as W
import           XMonad.Util.NamedScratchpad

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "terminalSR" spawnTermSR       findTermSR      manageTermSR       -- Small terminal scratchpad
  , NS "terminalBR" spawnTermBR       findTermBR      manageTermBR       -- Big terminal scratchpad
  , NS "terminalBL" spawnTermBL       findTermBL      manageTermBL       -- Big terminal scratchpad
  , NS "terminalSL" spawnTermSL       findTermSL      manageTermSL       -- Small terminal scratchpad
  , NS "FM"         spawnFileManager  findFileManager manageFileManager  -- File manager scratchpad
  , NS "WebA"       spawnWebA         findWebA        manageWebA         -- Chromium small scratchpad
  , NS "WebB"       spawnWebB         findWebB        manageWebB         -- Chromium big scratchpad
  ]

 where
  role         = stringProperty "WM_WINDOW_ROLE"
  spawnTermSR  = myTerminal ++ " start --class Scratchpad_Small_Right"  -- launch terminal
  findTermSR   = className =? "Scratchpad_Small_Right"                  -- its window will be named "Scratchpad_Small_Right"
  manageTermSR = customFloating $ W.RationalRect l t w h                -- geometry
   where
    h = 0.15          -- height 15%
    w = 0.57          -- width 57%
    t = 0.98 - h      -- bottom 2%
    l = 0.99 - w      -- right 1%

  spawnTermBR  = myTerminal ++ " start --class Scratchpad_Big_Right"    -- launch terminal
  findTermBR   = className =? "Scratchpad_Big_Right"                    -- its window will be named "Scratchpad_Big_Right"
  manageTermBR = customFloating $ W.RationalRect l t w h                -- geometry
   where
    h = 0.78         -- height 78%
    w = 0.57         -- width 57%
    t = 0.03         -- top 3%
    l = 0.99 - w     -- right 1%

  spawnTermSL  = myTerminal ++ " start --class Scratchpad_Small_Left"  -- launch terminal
  findTermSL   = className =? "Scratchpad_Small_Left"                  -- its window will be named "Scratchpad_Small_Left"
  manageTermSL = customFloating $ W.RationalRect l t w h               -- geometry
   where
    h = 0.31         -- height 31%
    w = 0.40         -- width 40%
    t = 0.98 - h     -- bottom 2%
    l = 0.01         -- left 1%

  spawnTermBL  = myTerminal ++ " start --class Scratchpad_Big_Left"    -- launch terminal
  findTermBL   = className =? "Scratchpad_Big_Left"                    -- its window will be named "Scratchpad_Big_Left"
  manageTermBL = customFloating $ W.RationalRect l t w h               -- geometry
   where
    h = 0.62        -- height 62%
    w = 0.40        -- width 40%
    t = 0.03        -- top 3%
    l = 0.01        -- left 1%

  spawnFileManager  = myTerminal ++ " start --class FileManager nnn"   -- launch n3 file manager
  findFileManager   = resource =? "FileManager"                        -- its window will be named "FileManager"
  manageFileManager = customFloating $ W.RationalRect l t w h          -- geometry
   where
    h = 0.78        -- height 78%
    w = 0.98        -- width 98%
    t = 0.03        -- top 3%
    l = (1 - w) / 2 -- centered left/right

  spawnWebA  = "chromium --restore-last-session"
  findWebA   = className =? "Chromium"
  manageWebA = customFloating $ W.RationalRect l t w h                 -- geometry:
   where
    h = 0.31       -- height 31%
    w = 0.40       -- width 40%
    t = 0.98 - h   -- bottom 2%
    l = 0.01       -- left 1%

  spawnWebB  = "firefox-developer-edition --new-window http://localhost:3000 --devtools"
  findWebB   = className =? "firefoxdeveloperedition"
  manageWebB = customFloating $ W.RationalRect l t w h                 -- geometry:
   where
    h = 0.78       -- height 78%
    w = 0.57       -- width 57%
    t = 0.03       -- top 3%
    l = 0.99 - w   -- right 1%


scratchTermSR :: X ()
scratchTermSR = namedScratchpadAction myScratchpads "terminalSR"
scratchTermBR :: X ()
scratchTermBR = namedScratchpadAction myScratchpads "terminalBR"
scratchTermSL :: X ()
scratchTermSL = namedScratchpadAction myScratchpads "terminalSL"
scratchTermBL :: X ()
scratchTermBL = namedScratchpadAction myScratchpads "terminalBL"
scratchFM :: X ()
scratchFM = namedScratchpadAction myScratchpads "FM"
scratchWebA :: X ()
scratchWebA = namedScratchpadAction myScratchpads "WebA"
scratchWebB :: X ()
scratchWebB = namedScratchpadAction myScratchpads "WebB"
