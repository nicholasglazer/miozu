------------------------------------------------------------------------
-- AllScratchpads: Combines regular scratchpads with AI scratchpads
------------------------------------------------------------------------
module AllScratchpads (allScratchpads) where

import XMonad.Util.NamedScratchpad
import Scratchpads (myScratchpads)
import AiAgents (myAiScratchpads)

-- Combine all scratchpads
allScratchpads :: [NamedScratchpad]
allScratchpads = myScratchpads ++ myAiScratchpads