------------------------------------------------------------------------
-- ScratchPads:
--
-- Useful and convenient tool to quickly launch programs in predictible
-- places above all windows, hide them after using same keybinding.
-- more info in article https://pbrisbin.com/posts/scratchpad_everything/
------------------------------------------------------------------------
module Scratchpads
  ( myScratchpads
  , scratchTermS
  , scratchTermB
  , scratchFM
  ) where

import           Variables                      ( myTerminal )
import           XMonad
import qualified XMonad.StackSet               as W
import           XMonad.Util.NamedScratchpad

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "terminalS" spawnTermS       findTermS       manageTermS       -- Small terminal scratchpad
  , NS "terminalB" spawnTermB       findTermB       manageTermB       -- Big terminal scratchpad
  , NS "FM"        spawnFileManager findFileManager manageFileManager -- File manager scratchpad
  -- , NS "mpd"       spawnMpd         findMpd          manageMpd         -- Mpd scratchpad
  -- , NS "mixer"     spawnMixer       findMixer        manageMixer       -- Mixer scratchpad
  ]

 where
  role        = stringProperty "WM_WINDOW_ROLE"
  spawnTermS  = myTerminal ++ " start --class Scratchpad_Small"  -- launch terminal
  findTermS   = className =? "Scratchpad_Small"                  -- its window will be named "Scratchpad_Small"
  manageTermS = customFloating $ W.RationalRect l t w h          -- geometry
   where
    h = 0.15          -- height 15%
    w = 0.98          -- width 98%
    t = 0.98 - h      -- top 98%
    l = (1 - w) / 2   -- centered left/right

  spawnTermB  = myTerminal ++ " start --class Scratchpad_Big"    -- launch terminal
  findTermB   = className =? "Scratchpad_Big"                    -- its window will be named "Scratchpad_Big"
  manageTermB = customFloating $ W.RationalRect l t w h          -- geometry
   where
    h = 0.78         -- height 78%
    w = 0.57         -- width 57%
    t = 0.03         -- top 3%
    l = 0.99 - w     -- left 99%

  spawnFileManager  = myTerminal ++ " start --class FileManager nnn" -- launch n3 file manager
  findFileManager   = resource =? "FileManager"                      -- its window will be named "FileManager"
  manageFileManager = customFloating $ W.RationalRect l t w h        -- geometry
   where
    h = 0.78        -- height 78%
    w = 0.98        -- width 98%
    t = 0.03        -- top 3%
    l = (1 - w) / 2 -- centered left/right

  -- spawnMixer  = "urxvt -name Pulsemixer -e pulsemixer"          -- launch pulsemixer
  -- findMixer   = resource =? "Pulsemixer"                        -- its window will be named "pulsemixer"
  -- manageMixer = customFloating $ W.RationalRect l t w h         -- geometry:
  --  where
  --   h = 0.1         -- height 10%
  --   w = 0.40        -- width  40%
  --   t = 0.03        -- top 3%
  --   l = 0.01        -- left 1%

  -- spawnMpd  = "urxvt -name MPD -e pms"                          -- launch pmus-git
  -- findMpd   = resource =? "MPD"                                 -- its window will be named "mpd"
  -- manageMpd = customFloating $ W.RationalRect l t w h           -- geometry:
  --  where
  --   h = 0.66        -- height, 66%
  --   w = 0.40        -- width,  40%
  --   t = 0.15        -- top 15%
  --   l = 0.01        -- left 1%


scratchTermS :: X ()
scratchTermS = namedScratchpadAction myScratchpads "terminalS"
scratchTermB :: X ()
scratchTermB = namedScratchpadAction myScratchpads "terminalB"
scratchFM :: X ()
scratchFM = namedScratchpadAction myScratchpads "FM"
-- scratchMixer :: X ()
-- scratchMixer = namedScratchpadAction myScratchpads "mixer"
-- scratchMusic :: X ()
-- scratchMusic = namedScratchpadAction myScratchpads "mpd"
