
------------------------------------------------------------------------
-- Workspaces
--
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
-- > workspaces = ["ws1", "ws2", "ws3" ] ++ map show [4..9]
--
-- This particular gliphs are japanese symbols that are represents numbers
------------------------------------------------------------------------
-- Export all workspaces to increase readability and avoid call by index
module Workspaces
  ( webWS
  , emacsWS
  , workWS
  , termWS
  , mediaWS
  , socialWS
  , toolsWS
  , magicWS
  , privateWS
  , myWorkspaces
  ) where

webWS :: String
webWS = " <fn=1>一</fn> "

emacsWS :: String
emacsWS = " <fn=1>二</fn> "

workWS :: String
workWS = " <fn=1>三</fn> "

termWS :: String
termWS = " <fn=1>四</fn> "

mediaWS :: String
mediaWS = " <fn=1>五</fn> "

socialWS :: String
socialWS = " <fn=1>六</fn> "

toolsWS :: String
toolsWS = " <fn=1>七</fn> "

magicWS :: String
magicWS = " <fn=1>八</fn> "

privateWS :: String
privateWS = " <fn=1>九</fn> "

myWorkspaces :: [String]
myWorkspaces =
  [ webWS
  , emacsWS
  , workWS
  , termWS
  , mediaWS
  , socialWS
  , toolsWS
  , magicWS
  , privateWS
  ]
