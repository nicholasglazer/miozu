------------------------------------------------------------------------ --
--Layouts: -- -- You can specify and transform your layouts by modifying these
--values.  -- If you change layout bindings be sure to use 'mod-shift-space'
--after -- restarting (with 'mod-q') to reset your layout state to the new --
--defaults, as xmonad preserves your old layout settings by default.  -- The
--available layouts. Note that each layout is separated by |||,
-- which denotes layout choice.
------------------------------------------------------------------------
module Layouts
  ( mediaLayout,
    workLayout,
    defLayout,
    leftHalfMasterLayout,
    workspace2Layout,
    experimentalLayout,
    myLayout,
  )
where

import XMonad
import XMonad.Hooks.ManageDocks -- This module provides tools to automatically manage dock type programs, such as gnome-panel, kicker, dzen, and xmobar.
import XMonad.Layout (Choose, Full (..), Mirror (..), Tall (Tall), (|||))
import XMonad.Layout.Accordion (Accordion (..))
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

-- Professional layouts for intensive workflows
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.Columns (ColumnsLayout (Columns))
import XMonad.Layout.IfMax (IfMax (IfMax))
import XMonad.Layout.Magnifier (magnifiercz', magnifier)
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Layout.OneBig (OneBig (OneBig))
import XMonad.Layout.ResizableTile (ResizableTall (..))
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.SubLayouts (subTabbed)
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.WindowNavigation (windowNavigation)

-- Fullscreen layout without borders, and circle layout.
mediaLayout = noBorders $ Full

-- Tiled, fullscreen, and accordion layouts with gaps.
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
defLayout = gap ||| noBorders Full ||| Grid ||| noBorders Accordion
  where
    nmaster = 1 -- The default number of windows in the master pane
    ratio = 0.5 -- Default proportion of screen occupied by master pane
    delta = 0.3 -- Percent of screen to increment by when resizing panes
    gap =
      spacingRaw True (Border 10 20 20 20) True (Border 20 20 20 20) True $
        ThreeColMid nmaster delta ratio -- tiling algo partitions the screen into three columns with spacing

-- Left-half master with right-side stacked layout for workspaces 1, 3, 4
-- Master window takes exactly 50% (left half), other windows stack vertically on right
leftHalfMasterLayout = leftHalf ||| gapLeftHalf ||| resizableLeftHalf ||| noBorders Full ||| Grid
  where
    nmaster = 1 -- One master window on left
    ratio = 0.5 -- Exactly half screen for master (left column)
    delta = 0.03 -- 3% resize steps
    leftHalf = Tall nmaster delta ratio -- Classic tall with 50% master
    gapLeftHalf =
      spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $
        Tall nmaster delta ratio -- Tall with gaps
    resizableLeftHalf = ResizableTall nmaster delta ratio [] -- Individually resizable windows

-- Workspace 2 specific layout (Emacs/Chromium workspace)
-- 50% master, 63% master with gaps
workspace2Layout = leftHalf ||| master63WithGaps ||| noBorders Full ||| Grid
  where
    nmaster = 1 -- One master window on left
    delta = 0.03 -- 3% resize steps
    -- 50% master layout (no gaps)
    leftHalf = Tall nmaster delta 0.5
    -- 63% master layout with small vertical gaps on right stack
    master63WithGaps =
      spacingRaw True (Border 0 0 0 0) True (Border 5 0 5 0) True $
        Tall nmaster delta 0.63

-- Experimental professional layouts for workspace 6 (testing)
-- Collection of advanced layouts optimized for 10+ windows on single monitor
experimentalLayout =
  windowNavigation . boringWindows . subTabbed $
    gap bsp |||              -- BSP: Manual binary space partitioning control
    gap cols |||             -- Columns: Modern multi-column layout
    gap resizableTall |||    -- ResizableTall: Individual window resizing
    gap spiralLayout |||     -- Spiral: Focus on main window, decreasing sizes
    gap onebig |||           -- OneBig: One large master + small auxiliary windows
    gap magnifierTall |||    -- Magnifier: Auto-enlarge focused window
    gap multicol |||         -- MultiColumns: Automatic column distribution
    gap threecol |||         -- ThreeColumns: Center master with side columns
    simpleTabbed |||         -- Tabbed: All windows as tabs
    noBorders Full |||       -- Fullscreen: No distractions
    Grid                     -- Grid: Equal-sized windows
  where
    nmaster = 1
    ratio = 0.5
    delta = 0.03
    -- Base layouts
    bsp = emptyBSP
    cols = Columns 1 []
    resizableTall = ResizableTall nmaster delta ratio []
    spiralLayout = spiral (6/7) -- Golden ratio
    onebig = OneBig (3/4) (3/4) -- Master takes 3/4 width and height
    magnifierTall = magnifiercz' 1.3 (Tall nmaster delta ratio) -- 30% magnification
    multicol = multiCol [1] 2 0.01 (-0.5) -- Max 2 windows per column
    threecol = ThreeColMid nmaster delta ratio
    -- Spacing helper
    gap = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True

myLayout =
  avoidStruts . smartBorders $
    windowArrange $
      onWorkspace " <fn=1>一</fn> " leftHalfMasterLayout $ -- Workspace 1: web
        onWorkspace " <fn=1>二</fn> " workspace2Layout $ -- Workspace 2: emacs/chromium (50%/67%/68%)
          onWorkspace " <fn=1>三</fn> " leftHalfMasterLayout $ -- Workspace 3: work
            onWorkspace " <fn=1>四</fn> " leftHalfMasterLayout $ -- Workspace 4: term
              onWorkspace " <fn=1>五</fn> " mediaLayout $ -- Workspace 5: media
                onWorkspace " <fn=1>六</fn> " experimentalLayout $ -- Workspace 6: social (EXPERIMENTAL)
                  defLayout
