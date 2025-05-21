#!/bin/bash
# xmonad.sh - Minimal script to build XMonad using Stack
# Author: Nicholas Glazer

set -e

# Config
XMONAD_DIR="$HOME/.config/xmonad"
mkdir -p "$XMONAD_DIR"
cd "$XMONAD_DIR"

# Clone repositories if needed
[ -d xmonad ] || git clone https://github.com/xmonad/xmonad.git
[ -d xmonad-contrib ] || git clone https://github.com/xmonad/xmonad-contrib.git

# Create stack config
cat > stack.yaml << EOF
resolver: lts-21.25
packages:
  - xmonad/
  - xmonad-contrib/
EOF

# Create build script
mkdir -p bin
cat > bin/build << EOF
#!/bin/bash
cd "$XMONAD_DIR"
stack build xmonad xmonad-contrib --copy-bins --local-bin-path="$HOME/.local/bin"
EOF
chmod +x bin/build

# Build
mkdir -p "$HOME/.local/bin"
./bin/build

echo "XMonad built successfully. Binaries in ~/.local/bin"
