--------------------------------------------------------------------------------------
-- Projects:
--
-- DynamicProjects allows you to dedicate workspaces to specific projects
-- and then switch between projects easily.
-- A project is made up of a name, working directory, and a start-up hook.
-- When you switch to a workspace, DynamicProjects changes the working directory
-- to the one configured for the matching project.
-- If the workspace doesn't have any windows, the project's start-up hook is executed.
-- This allows you to launch applications or further configure the workspace/project.
---------------------------------------------------------------------------------------
module Projects
  ( Project
  , myProjects
  ) where

import Variables                      ( myTerminal
                                      , myTextEditor
                                      )
import Workspaces                     ( emacsWS
                                      , termWS
                                      , dualWS
                                      )
import XMonad                         ( X
                                      , spawn
                                      )
import XMonad.Actions.DynamicProjects ( Project(..) )

myProjects :: [Project]
myProjects =
  [ Project
    { projectName      = "scratch"
    , projectDirectory = "~/"
    , projectStartHook = Nothing
    }
  , Project
    { projectName      = emacsWS
    , projectDirectory = "~/code"
    , projectStartHook = Just $ do
                           spawn myTextEditor
    }
  , Project
    { projectName      = termWS
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           spawn myTerminal
    }
  , Project
    { projectName      = dualWS
    , projectDirectory = "~/Videos"
    , projectStartHook = Just $ do
                           spawn "chromium"
    }
  ]
