--   INFO
--  ------
-- meta = {
--          Updated = May 13 2023
--        , Version = xmonad 0.17.1 <+> ghc 9.0.2
--        , Author  = Nicholas Glazer
--        }
--
-- This XMonad file is the heart of miozu config.
-- Read more at https://miozu.com/documentation
--
--- General Imports
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Hooks.UrgencyHook (withUrgencyHook)
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.DynamicProjects (dynamicProjects)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Hooks.FadeWindows (fadeWindowsLogHook)
-- Import /lib modules
import Hooks (myFadeHook, myEventHook, myStartupHook, LibNotifyUrgencyHook (..), myLogHook)
import Keybindings (myKeys)
import Layouts (myLayout)
import Projects (myProjects)
import Scratchpads (myScratchpads)
import Variables (myBar, myFocusedBorderColor, myNormalBorderColor, myTerminal, myFocusFollowsMouse, myClickJustFocuses, myBorderWidth, myModMask)
import Workspaces (webWS, emacsWS, workWS, termWS, mediaWS, socialWS, toolsWS, magicWS, privateWS, myWorkspaces)
import WorkspaceRules (myManageHook)

------------------------------------------------------------------------
-- Main function:
--
-- Run xmonad with all specified settings.
------------------------------------------------------------------------
main = do
  xmprocs <- spawnPipe myBar
  xmonad
    $ withUrgencyHook LibNotifyUrgencyHook
    $ dynamicProjects myProjects
    $ fullscreenSupport
    $ docks
    $ ewmh defaults {
    logHook = composeAll [
        fadeWindowsLogHook myFadeHook
        , myLogHook xmprocs
        ]
    , focusedBorderColor = myFocusedBorderColor
    , normalBorderColor  = myNormalBorderColor
    }

defaults = def {
  terminal            = myTerminal,
  focusFollowsMouse   = myFocusFollowsMouse,
  clickJustFocuses    = myClickJustFocuses,
  borderWidth         = myBorderWidth,
  modMask             = myModMask,
  workspaces          = myWorkspaces,
  keys                = myKeys,
  layoutHook          = myLayout,
  manageHook          = myManageHook,
  handleEventHook     = myEventHook,
  startupHook         = myStartupHook
  }

