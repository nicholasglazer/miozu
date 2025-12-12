# XMonad Build Analysis for Miozu

## Current Build Setup

### Package Installation Method
Your `install.sh` script installs XMonad from the AUR (Arch User Repository):
- **File**: `bin/dependencies/required-packages.txt` (lines 12-14)
- **Packages**: 
  ```
  xmonad
  xmonad-contrib
  xmobar
  ```
- These are installed via `paru` along with other required packages

### Stack Build Script
You also have a separate Stack-based build script:
- **File**: `bin/xmonad.sh`
- **Purpose**: Builds XMonad from source using Stack
- **Process**:
  1. Clones xmonad and xmonad-contrib from GitHub
  2. Creates a `stack.yaml` with resolver `lts-21.25`
  3. Builds and installs binaries to `~/.local/bin`

## Build Method Detection

When you run `xmonad --recompile`:
1. XMonad checks for build configuration in this order:
   - `/home/ng/.config/xmonad/build`
   - `/home/ng/.config/xmonad/stack.yaml`
   - `/home/ng/.config/xmonad/flake.nix`
   - `/home/ng/.config/xmonad/default.nix`

2. If `stack.yaml` exists (created by `xmonad.sh`), it uses Stack
3. Otherwise, it falls back to plain GHC

## Current Status
- The error messages show XMonad is trying to use Stack: `"XMonad will use stack ghc --stack-yaml..."`
- This means `xmonad.sh` was run at some point, creating the Stack configuration
- However, `stack` command is not found in PATH, causing compilation failures

## Recommendations

### Option 1: Use AUR XMonad (Simpler)
```bash
# Remove Stack configuration
rm -f ~/.config/xmonad/stack.yaml
rm -rf ~/.config/xmonad/xmonad ~/.config/xmonad/xmonad-contrib

# Recompile with system GHC
xmonad --recompile
```

### Option 2: Use Stack Build (More Control)
```bash
# Install Stack
paru -S stack

# Run your build script
~/.miozu/bin/xmonad.sh

# Recompile
xmonad --recompile
```

### Option 3: Hybrid Approach
Keep AUR packages but add Stack for development:
- Use AUR xmonad for stability
- Use Stack when testing new features
- Switch between them by adding/removing `stack.yaml`

## AI Agent Integration Status
The AI agent changes are ready but need successful compilation:
- New module: `lib/AiAgents.hs`
- New keybindings: Mod+Ctrl+{a,x,w,s}
- Integration with existing scratchpad system
- Designed for `claude-code` command integration