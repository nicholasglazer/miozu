#!/bin/bash
# XMonad Stack build functions for Miozu installer
# Based on official XMonad documentation: https://xmonad.org/INSTALL.html

source "$SCRIPT_DIR/lib/common.sh"

# XMonad configuration
XMONAD_CONFIG_DIR="$HOME/.config/xmonad"
XMONAD_DATA_DIR="$HOME/.local/share/xmonad"
XMONAD_CACHE_DIR="$HOME/.cache/xmonad"
XMONAD_REPO="https://github.com/xmonad/xmonad.git"
XMONAD_CONTRIB_REPO="https://github.com/xmonad/xmonad-contrib.git"

# Setup XMonad with Stack (following official docs)
setup_xmonad_stack() {
    print_header "Setting up XMonad with Stack"
    
    # Create necessary directories
    mkdir -p "$XMONAD_CONFIG_DIR"
    mkdir -p "$XMONAD_DATA_DIR"
    mkdir -p "$XMONAD_CACHE_DIR"
    
    cd "$XMONAD_CONFIG_DIR" || exit 1
    
    # Clone or update repositories
    if [[ -d "xmonad" ]]; then
        echo "Updating xmonad repository..."
        (cd xmonad && git pull)
    else
        echo "Cloning xmonad repository..."
        git clone "$XMONAD_REPO"
    fi
    
    if [[ -d "xmonad-contrib" ]]; then
        echo "Updating xmonad-contrib repository..."
        (cd xmonad-contrib && git pull)
    else
        echo "Cloning xmonad-contrib repository..."
        git clone "$XMONAD_CONTRIB_REPO"
    fi
    
    # Create stack.yaml based on official docs
    echo "Creating stack.yaml..."
    cat > stack.yaml << 'EOF'
# Stack configuration for XMonad
# Based on https://xmonad.org/INSTALL.html

resolver: lts-21.25

packages:
- xmonad
- xmonad-contrib

extra-deps:
# Add any additional dependencies here if needed

# Flags for xmonad-contrib
flags:
  xmonad-contrib:
    use_xft: true

# Use system libraries
extra-include-dirs:
- /usr/include

extra-lib-dirs:
- /usr/lib

# Enable pedantic mode for better error messages
ghc-options:
  "$locals": -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints
EOF
    
    # Create build script as recommended by XMonad docs
    echo "Creating build script..."
    mkdir -p "$XMONAD_CONFIG_DIR/bin"
    cat > "$XMONAD_CONFIG_DIR/build" << 'EOF'
#!/bin/bash
# XMonad build script using Stack
# This script is called by xmonad --recompile

set -e

cd "$HOME/.config/xmonad"

# Build XMonad and XMonad-contrib
echo "Building XMonad with Stack..."
stack build \
    --local-bin-path="$HOME/.local/bin" \
    --copy-bins \
    --copy-compiler-tool \
    xmonad xmonad-contrib

# The actual compilation of xmonad.hs is handled by xmonad itself
# We just need to ensure the libraries are built
echo "XMonad libraries built successfully"
EOF
    chmod +x "$XMONAD_CONFIG_DIR/build"
    
    # Create a simple build-env script for development
    cat > "$XMONAD_CONFIG_DIR/bin/build-env" << 'EOF'
#!/bin/bash
# Enter a Stack environment for XMonad development
cd "$HOME/.config/xmonad"
exec stack exec -- "$SHELL"
EOF
    chmod +x "$XMONAD_CONFIG_DIR/bin/build-env"
    
    echo -e "${GREEN}XMonad Stack setup complete${NC}"
}

# Build XMonad using Stack
build_xmonad_stack() {
    print_header "Building XMonad with Stack"
    
    cd "$XMONAD_CONFIG_DIR" || exit 1
    
    # First, ensure Stack dependencies are installed
    echo "Installing Stack dependencies..."
    stack setup
    
    # Build XMonad and XMonad-contrib
    echo "Building XMonad and XMonad-contrib..."
    stack build xmonad xmonad-contrib \
        --local-bin-path="$HOME/.local/bin" \
        --copy-bins \
        --copy-compiler-tool
    
    # Create xmonad wrapper script
    echo "Creating XMonad wrapper..."
    cat > "$HOME/.local/bin/xmonad" << 'EOF'
#!/bin/bash
# XMonad wrapper script for Stack installation
export XMONAD_CONFIG_DIR="$HOME/.config/xmonad"
export XMONAD_DATA_DIR="$HOME/.local/share/xmonad"
export XMONAD_CACHE_DIR="$HOME/.cache/xmonad"

# Use Stack's GHC for recompilation
export PATH="$HOME/.local/bin:$PATH"

# Execute the real xmonad binary built by Stack
exec "$HOME/.local/bin/xmonad-exe" "$@"
EOF
    chmod +x "$HOME/.local/bin/xmonad"
    
    echo -e "${GREEN}XMonad built successfully${NC}"
}

# Compile XMonad configuration
compile_xmonad_config() {
    print_header "Compiling XMonad Configuration"
    
    cd "$XMONAD_CONFIG_DIR" || exit 1
    
    # Ensure error file exists
    mkdir -p "$XMONAD_DATA_DIR"
    touch "$XMONAD_DATA_DIR/xmonad.errors"
    
    # Use xmonad --recompile which will use our build script
    echo "Recompiling XMonad configuration..."
    if xmonad --recompile; then
        echo -e "${GREEN}XMonad configuration compiled successfully${NC}"
        
        # Test the configuration
        if xmonad --test; then
            echo -e "${GREEN}XMonad configuration test passed${NC}"
        else
            echo -e "${YELLOW}XMonad configuration test failed - check your config${NC}"
        fi
    else
        echo -e "${RED}XMonad compilation failed${NC}"
        echo "Check $XMONAD_DATA_DIR/xmonad.errors for details"
        return 1
    fi
}

# Verify XMonad Stack installation
verify_xmonad_stack() {
    print_header "Verifying XMonad Stack Installation"
    
    local errors=0
    
    # Check directories
    for dir in "$XMONAD_CONFIG_DIR" "$XMONAD_DATA_DIR" "$XMONAD_CACHE_DIR"; do
        if [[ -d "$dir" ]]; then
            echo -e "${GREEN}✓ Directory exists: $dir${NC}"
        else
            echo -e "${RED}✗ Missing directory: $dir${NC}"
            ((errors++))
        fi
    done
    
    # Check Stack files
    if [[ -f "$XMONAD_CONFIG_DIR/stack.yaml" ]]; then
        echo -e "${GREEN}✓ stack.yaml exists${NC}"
    else
        echo -e "${RED}✗ stack.yaml missing${NC}"
        ((errors++))
    fi
    
    # Check build script
    if [[ -x "$XMONAD_CONFIG_DIR/build" ]]; then
        echo -e "${GREEN}✓ build script exists and is executable${NC}"
    else
        echo -e "${RED}✗ build script missing or not executable${NC}"
        ((errors++))
    fi
    
    # Check XMonad binary
    if command -v xmonad &>/dev/null; then
        echo -e "${GREEN}✓ xmonad command available${NC}"
        xmonad --version || true
    else
        echo -e "${RED}✗ xmonad command not found${NC}"
        ((errors++))
    fi
    
    # Check if XMonad libraries are built
    if stack exec -- ghc-pkg list | grep -q xmonad; then
        echo -e "${GREEN}✓ XMonad libraries installed in Stack${NC}"
    else
        echo -e "${RED}✗ XMonad libraries not found in Stack${NC}"
        ((errors++))
    fi
    
    if [[ $errors -eq 0 ]]; then
        echo -e "${GREEN}All XMonad Stack checks passed!${NC}"
        return 0
    else
        echo -e "${RED}XMonad Stack setup has $errors error(s)${NC}"
        return 1
    fi
}

# Clean XMonad build artifacts
clean_xmonad_build() {
    print_header "Cleaning XMonad Build"
    
    echo "Removing build artifacts..."
    rm -rf "$XMONAD_CACHE_DIR"/*
    rm -f "$XMONAD_DATA_DIR/xmonad.errors"
    rm -f "$XMONAD_DATA_DIR/xmonad.state"
    rm -f "$XMONAD_CONFIG_DIR/.stack-work"
    
    echo -e "${GREEN}XMonad build cleaned${NC}"
}