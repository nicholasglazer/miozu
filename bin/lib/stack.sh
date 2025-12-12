#!/bin/bash
# Stack and Haskell setup functions for Miozu installer
# Based on official XMonad documentation: https://xmonad.org/INSTALL.html

source "$SCRIPT_DIR/lib/common.sh"

# Install Stack if not present
install_stack() {
    if command -v stack &>/dev/null; then
        echo -e "${GREEN}Stack already installed${NC}"
        return 0
    fi
    
    print_header "Installing Stack"
    
    # Method 1: Try AUR first (preferred for Arch)
    if command -v paru &>/dev/null; then
        echo "Installing Stack from AUR..."
        paru -S --noconfirm --needed stack-bin || {
            echo -e "${YELLOW}AUR installation failed, trying official installer...${NC}"
            install_stack_official
        }
    else
        install_stack_official
    fi
    
    # Verify installation
    if command -v stack &>/dev/null; then
        echo -e "${GREEN}Stack installed successfully${NC}"
        stack --version
    else
        echo -e "${RED}Stack installation failed${NC}"
        return 1
    fi
}

# Official Stack installer method
install_stack_official() {
    echo "Using official Stack installer..."
    curl -sSL https://get.haskellstack.org/ | sh
    
    # Add to PATH if needed
    if [[ ! ":$PATH:" == *":$HOME/.local/bin:"* ]]; then
        echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
        echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.config/fish/config.fish 2>/dev/null || true
        export PATH="$HOME/.local/bin:$PATH"
    fi
}

# Setup Stack for XMonad development
setup_stack_xmonad() {
    print_header "Setting up Stack for XMonad"
    
    # Install GHC and build tools via Stack
    echo "Installing GHC and build tools..."
    stack setup
    
    # Install useful Haskell tools
    echo "Installing Haskell development tools..."
    stack install --resolver lts-21.25 \
        hlint \
        haskell-language-server \
        ormolu \
        implicit-hie || true
    
    # Configure Stack globally
    mkdir -p ~/.stack
    cat > ~/.stack/config.yaml << 'EOF'
# Stack global configuration
templates:
  params:
    author-name: Nicholas Glazer
    author-email: glazer.nicholas@gmail.com
    copyright: Nicholas Glazer
    github-username: ng

# Use system GHC if available
system-ghc: false
install-ghc: true

# Build settings
build:
  copy-bins: true
  
# Set local bin path
local-bin-path: /home/ng/.local/bin

# Extra directories
extra-include-dirs:
- /usr/include
extra-lib-dirs:
- /usr/lib
EOF
    
    echo -e "${GREEN}Stack configured for XMonad development${NC}"
}

# Verify Stack installation and configuration
verify_stack_setup() {
    print_header "Verifying Stack Setup"
    
    local errors=0
    
    # Check Stack
    if ! command -v stack &>/dev/null; then
        echo -e "${RED}✗ Stack not found in PATH${NC}"
        ((errors++))
    else
        echo -e "${GREEN}✓ Stack installed: $(stack --version)${NC}"
    fi
    
    # Check GHC
    if ! stack exec -- ghc --version &>/dev/null; then
        echo -e "${RED}✗ GHC not available via Stack${NC}"
        ((errors++))
    else
        echo -e "${GREEN}✓ GHC available: $(stack exec -- ghc --version | head -1)${NC}"
    fi
    
    # Check ~/.local/bin in PATH
    if [[ ! ":$PATH:" == *":$HOME/.local/bin:"* ]]; then
        echo -e "${YELLOW}⚠ ~/.local/bin not in PATH${NC}"
        echo "  Add to your shell configuration:"
        echo "  export PATH=\"\$HOME/.local/bin:\$PATH\""
    else
        echo -e "${GREEN}✓ ~/.local/bin in PATH${NC}"
    fi
    
    # Check if we can compile a simple Haskell program
    echo "Testing Haskell compilation..."
    local test_file=$(mktemp --suffix=.hs)
    cat > "$test_file" << 'EOF'
main :: IO ()
main = putStrLn "Stack setup successful!"
EOF
    
    if stack exec -- ghc "$test_file" -o /tmp/stack_test &>/dev/null && /tmp/stack_test &>/dev/null; then
        echo -e "${GREEN}✓ Haskell compilation working${NC}"
    else
        echo -e "${RED}✗ Haskell compilation failed${NC}"
        ((errors++))
    fi
    
    rm -f "$test_file" /tmp/stack_test
    
    if [[ $errors -eq 0 ]]; then
        echo -e "${GREEN}All Stack checks passed!${NC}"
        return 0
    else
        echo -e "${RED}Stack setup has $errors error(s)${NC}"
        return 1
    fi
}