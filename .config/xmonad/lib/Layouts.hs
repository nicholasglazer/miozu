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
module Layouts
  ( mediaLayout,
    workLayout,
    defLayout,
    myLayout,
  )
where

import XMonad
import XMonad.Hooks.ManageDocks -- This module provides tools to automatically manage dock type programs, such as gnome-panel, kicker, dzen, and xmobar.
import XMonad.Layout (Choose, Full (..), Mirror (..), Tall (Tall), (|||))
import XMonad.Layout.Accordion (Accordion (..))
import XMonad.Layout.Circle (Circle (..))
import XMonad.Layout.Fullscreen (FullscreenFull, fullscreenFull)
import XMonad.Layout.Grid (Grid (Grid))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders (WithBorder, noBorders, smartBorders)
import XMonad.Layout.PerWorkspace (PerWorkspace, onWorkspace)
import XMonad.Layout.Spacing (Border (Border), Spacing, spacingRaw)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger (windowArrange)
-- TODO https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Layout-Magnifier.html
-- Magnifier layout seems nice.
-- import XMonad.Layout.Magnifier

-- Fullscreen layout without borders, and circle layout.
mediaLayout :: ModifiedLayout WithBorder (Choose (ModifiedLayout FullscreenFull Full) Circle) Window
mediaLayout = noBorders $ fullscreenFull Full ||| Circle

-- Tiled, fullscreen, and accordion layouts with gaps.
workLayout :: Choose Tall (Choose Full (Choose (ModifiedLayout Spacing Tall) (Choose (ModifiedLayout Spacing (Mirror Tall)) (ModifiedLayout WithBorder Accordion)))) Window
workLayout = tiled ||| Full ||| gap ||| gapM ||| noBorders Accordion
  where
    nmaster = 1 -- The default number of windows in the master pane
    ratio = 0.5 -- Default proportion of screen occupied by master pane
    delta = 0.3 -- Percent of screen to increment by when resizing panes
    tiled = Tall nmaster delta ratio -- tiling default tall column
    gapM =
      spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $
        Mirror $ Tall nmaster delta ratio -- tiling algo partitions the screen with spacing mirrored tall
    gap =
      spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $
        Tall nmaster delta ratio -- tiling algo partitions the screen with spacing tall

-- Three column mid layout with gaps, fullscreen layout without borders, circle layout, grid layout, and accordion layout.
defLayout :: Choose (ModifiedLayout Spacing ThreeCol) (Choose (ModifiedLayout WithBorder Full) (Choose Circle (Choose Grid (ModifiedLayout WithBorder Accordion)))) Window
defLayout = gap ||| noBorders Full ||| Circle ||| Grid ||| noBorders Accordion
  where
    nmaster = 1 -- The default number of windows in the master pane
    ratio = 0.5 -- Default proportion of screen occupied by master pane
    delta = 0.3 -- Percent of screen to increment by when resizing panes
    gap =
      spacingRaw True (Border 10 20 20 20) True (Border 20 20 20 20) True $
        ThreeColMid nmaster delta ratio -- tiling algo partitions the screen into three columns with spacing

myLayout =
  avoidStruts . smartBorders $
    windowArrange $
      onWorkspace "emacs" workLayout $
        onWorkspace "media" mediaLayout $
          onWorkspace "work" workLayout $
            defLayout
