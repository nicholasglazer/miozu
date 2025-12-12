-------------------------------------------------------------------------------
-- GridSelect config
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Actions-GridSelect.html
-------------------------------------------------------------------------------
module GridSelect (
  myColorizer,
  myGSConfig1,
  myGSConfig2
) where

import XMonad
import XMonad.Actions.GridSelect
import Miozu
import qualified Data.Map as M

-- Fixed colorizer without improper 'read' usage
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                     minBound -- lowest inactive bg
                     maxBound -- highest inactive bg
                     (0x3E, 0x43, 0x59) -- active bg (base2)
                     (0xD0, 0xD2, 0xDB) -- inactive fg (base5)
                     (0xFF, 0x99, 0x82) -- active fg (peach)

-- Main GridSelect configuration for window selection
myGSConfig1 :: (Window -> Bool -> X (String, String)) -> GSConfig Window
myGSConfig1 colorizer = def
    { gs_cellheight   = 80
    , gs_cellwidth    = 200
    , gs_cellpadding  = 8
    , gs_navigate    = navNSearch
    , gs_colorizer   = colorizer
    , gs_font        = "xft:Inter:size=10:bold:antialias=true"
    , gs_bordercolor = miozu02
    }

-- Alternative configuration for command/action selection
myGSConfig2 :: GSConfig String
myGSConfig2 = def
    { gs_cellheight   = 60
    , gs_cellwidth    = 250
    , gs_cellpadding  = 10
    , gs_navigate    = navNSearch
    , gs_font        = "xft:Inter:size=10:bold:antialias=true"
    , gs_bordercolor = miozu02
    }
