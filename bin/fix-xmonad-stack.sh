#!/bin/bash
# Quick fix for XMonad Stack compilation
# This script installs Stack and ensures XMonad can compile

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${YELLOW}=== XMonad Stack Quick Fix ===${NC}"

# Step 1: Install Stack
echo -e "\n${YELLOW}Step 1: Installing Stack...${NC}"
if command -v stack &>/dev/null; then
    echo -e "${GREEN}Stack already installed${NC}"
else
    echo "Installing Stack from AUR..."
    paru -S --noconfirm --needed stack-bin || {
        echo "Trying official installer..."
        curl -sSL https://get.haskellstack.org/ | sh
    }
fi

# Add to PATH if needed
if [[ ! ":$PATH:" == *":$HOME/.local/bin:"* ]]; then
    export PATH="$HOME/.local/bin:$PATH"
fi

# Step 2: Setup Stack
echo -e "\n${YELLOW}Step 2: Setting up Stack...${NC}"
stack setup

# Step 3: Fix XMonad directories
echo -e "\n${YELLOW}Step 3: Creating XMonad directories...${NC}"
mkdir -p ~/.local/share/xmonad
mkdir -p ~/.cache/xmonad
touch ~/.local/share/xmonad/xmonad.errors

# Step 4: Build XMonad libraries
echo -e "\n${YELLOW}Step 4: Building XMonad libraries...${NC}"
cd ~/.config/xmonad

# Ensure we have the latest stack.yaml
if [[ ! -f stack.yaml ]]; then
    echo "Creating stack.yaml..."
    cat > stack.yaml << 'EOF'
resolver: lts-21.25
packages:
- xmonad
- xmonad-contrib
EOF
fi

# Build the libraries
stack build xmonad xmonad-contrib --copy-bins --local-bin-path="$HOME/.local/bin"

# Step 5: Create build script
echo -e "\n${YELLOW}Step 5: Creating build script...${NC}"
cat > ~/.config/xmonad/build << 'EOF'
#!/bin/bash
set -e
cd ~/.config/xmonad
stack build --local-bin-path="$HOME/.local/bin" --copy-bins xmonad xmonad-contrib
echo "Build complete"
EOF
chmod +x ~/.config/xmonad/build

# Step 6: Test compilation
echo -e "\n${YELLOW}Step 6: Testing XMonad compilation...${NC}"
cd ~/.config/xmonad
if xmonad --recompile; then
    echo -e "${GREEN}✓ XMonad compiled successfully!${NC}"
else
    echo -e "${RED}✗ XMonad compilation failed${NC}"
    echo "Check ~/.local/share/xmonad/xmonad.errors for details"
    exit 1
fi

# Step 7: Verify
echo -e "\n${YELLOW}Step 7: Verification...${NC}"
stack exec -- ghc --version
xmonad --version || true

echo -e "\n${GREEN}=== XMonad Stack Fix Complete ===${NC}"
echo -e "${YELLOW}You can now:${NC}"
echo "1. Restart XMonad: xmonad --restart"
echo "2. Use AI agents: Mod+Ctrl+a (orchestrator), Mod+Ctrl+x (xmonad), etc."
echo "3. Recompile anytime: xmonad --recompile"