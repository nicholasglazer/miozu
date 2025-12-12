------------------------------------------------------------------------
-- AI Agents Integration for XMonad
--
-- Specialized scratchpads and window management for AI tools
------------------------------------------------------------------------
module AiAgents
  ( myAiScratchpads
  , aiAgentAction
  , aiOrchestratorAction
  ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.ExtensibleState as XS
import qualified Data.Map as M
import Variables (myTerminal)
import Scratchpads (myScratchpads)

-- State for tracking AI agent sessions
data AiAgentState = AiAgentState
  { activeAgents :: M.Map String String    -- agent name -> window id
  , agentContexts :: M.Map String String   -- agent name -> context/prompt
  } deriving (Typeable, Read, Show)

instance ExtensionClass AiAgentState where
  initialValue = AiAgentState M.empty M.empty

-- AI Agent scratchpads with optimized positioning
myAiScratchpads :: [NamedScratchpad]
myAiScratchpads =
  [ NS "ai_orchestrator" spawnOrchestrator findOrchestrator manageOrchestrator
  , NS "ai_xmonad"       spawnXmonadAgent  findXmonadAgent  manageXmonadAgent
  , NS "ai_webdev"       spawnWebdevAgent  findWebdevAgent  manageWebdevAgent
  , NS "ai_system"       spawnSystemAgent  findSystemAgent  manageSystemAgent
  ]
 where
  -- Orchestrator - central AI interface
  spawnOrchestrator = myTerminal ++ " start --class AI_Orchestrator -- claude-code"
  findOrchestrator = className =? "AI_Orchestrator"
  manageOrchestrator = customFloating $ W.RationalRect 0.15 0.1 0.7 0.8

  -- XMonad specialist
  spawnXmonadAgent = myTerminal ++ " start --class AI_XMonad -- claude-code --context xmonad"
  findXmonadAgent = className =? "AI_XMonad"
  manageXmonadAgent = customFloating $ W.RationalRect 0.01 0.03 0.48 0.45

  -- Web development specialist
  spawnWebdevAgent = myTerminal ++ " start --class AI_WebDev -- claude-code --context webdev"
  findWebdevAgent = className =? "AI_WebDev"
  manageWebdevAgent = customFloating $ W.RationalRect 0.51 0.03 0.48 0.45

  -- System/Arch specialist
  spawnSystemAgent = myTerminal ++ " start --class AI_System -- claude-code --context system"
  findSystemAgent = className =? "AI_System"
  manageSystemAgent = customFloating $ W.RationalRect 0.01 0.52 0.48 0.45

-- Launch or toggle AI agent
aiAgentAction :: String -> X ()
aiAgentAction agentName = do
  -- Track agent state
  state <- XS.get :: X AiAgentState
  
  -- Check if we need to set context
  case M.lookup agentName (agentContexts state) of
    Nothing -> do
      -- First launch - set context
      let context = getAgentContext agentName
      XS.modify $ \s -> s { agentContexts = M.insert agentName context (agentContexts s) }
    Just _ -> return ()
  
  -- Toggle the agent window
  namedScratchpadAction (myScratchpads ++ myAiScratchpads) ("ai_" ++ agentName)

-- Launch orchestrator with all agents
aiOrchestratorAction :: X ()
aiOrchestratorAction = do
  -- Launch orchestrator
  namedScratchpadAction (myScratchpads ++ myAiScratchpads) "ai_orchestrator"
  
  -- Optionally launch all specialists in background
  -- mapM_ aiAgentAction ["xmonad", "webdev", "system"]

-- Context templates for each agent
getAgentContext :: String -> String
getAgentContext "xmonad" = unlines
  [ "You are an XMonad expert for the Miozu setup."
  , "Config: Arch Linux, XMonad 0.17.1, Stack build"
  , "Modules: Variables.hs, Workspaces.hs, Projects.hs, Scratchpads.hs"
  , "Terminal: WezTerm, Editor: Emacs"
  , "Workspaces: Japanese symbols (一二三四五六七八九〇)"
  , "Scratchpads: Left Alt + Right Alt + 1-9"
  ]
getAgentContext "webdev" = unlines
  [ "You are a web development expert for the Miozu setup."
  , "Environment: Arch Linux, pnpm package manager"
  , "Global tools: n, npm-check-updates, nodemon, marked"
  , "Projects in ~/code with XMonad dynamic switching"
  , "Terminal: WezTerm with numbered scratchpads"
  ]
getAgentContext "system" = unlines
  [ "You are an Arch Linux system expert for the Miozu setup."
  , "Focus: Display management, system automation, shell scripts"
  , "Key scripts: bin/display/, monitor hotplug, systemd services"
  , "Package management: pacman/paru"
  ]
getAgentContext _ = "You are an AI assistant for the Miozu Linux environment."