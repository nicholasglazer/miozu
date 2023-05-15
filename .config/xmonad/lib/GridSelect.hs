-------------------------------------------------------------------------------
-- GridSelect config
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Actions-GridSelect.html
-------------------------------------------------------------------------------
module GridSelect (
  myColorizer,
  myGSConfig1
) where

import XMonad
import XMonad.Actions.GridSelect (GSConfig(..), colorRangeFromClassName, buildDefaultGSConfig)
import Miozu (red, green, blue, cyan, orange)


myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                     (read red) -- lowest inactive bg
                     (read green) -- highest inactive bg
                     (read blue) -- active bg
                     (read cyan) -- inactive fg
                     (read orange) -- active fg

--myGSConfig1 :: Window -> GSConfig Window
myGSConfig1 :: (Window -> Bool -> X (String, String)) -> GSConfig Window
myGSConfig1 colorizer = (buildDefaultGSConfig colorizer)
    { gs_cellheight   = 80
    , gs_cellwidth    = 200
    , gs_cellpadding  = 8
    , gs_bordercolor = red
    }
