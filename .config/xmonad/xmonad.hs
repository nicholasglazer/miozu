--   INFO
--  ------
-- meta = {
--          Updated = June 1 2023
--        , Version = xmonad 0.17.1 <+> ghc 9.0.2
--        , Author  = Nicholas Glazer
--        }
--
-- This XMonad file is the heart of miozu config.
-- Read more at https://miozu.com/documentation
--
--- General Imports
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Actions.DynamicProjects (dynamicProjects)
import XMonad.Hooks.StatusBar (withEasySB, withSB, defToggleStrutsKey, statusBarProp)
import qualified XMonad.Hooks.StatusBar as SB
-- Import /lib modules
import XMonad.Hooks.UrgencyHook (withUrgencyHook)
import Hooks (myEventHook, myStartupHook, myLogHook, LibNotifyUrgencyHook (..))
import Bar (mySB, myBottomSB)
import Keybindings (myKeys)
import Layouts (myLayout)
import Projects (myProjects)
import Scratchpads (myScratchpads)
import Variables (myFocusedBorderColor, myNormalBorderColor, myTerminal, myFocusFollowsMouse, myClickJustFocuses, myBorderWidth, myModMask)
import Workspaces (webWS, emacsWS, workWS, termWS, mediaWS, socialWS, toolsWS, magicWS, dualWS, myWorkspaces)
import WorkspaceRules (myManageHook)

------------------------------------------------------------------------
-- Main function:
--
-- Run xmonad with all specified settings.
------------------------------------------------------------------------

main :: IO()
main = xmonad
    $ withUrgencyHook LibNotifyUrgencyHook
    $ dynamicProjects myProjects
    $ docks
    . ewmhFullscreen
    . ewmh
    . withEasySB mySB defToggleStrutsKey
    . withSB myBottomSB
    $ defaults

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
  startupHook         = myStartupHook,
  logHook             = myLogHook
}
