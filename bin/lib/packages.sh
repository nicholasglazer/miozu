#!/bin/bash
# Package management functions for Miozu installer

# Check if running in WSL
_is_wsl() {
    grep -qi microsoft /proc/version 2>/dev/null
}

# Test if an AUR helper actually works (not just exists)
_test_aur_helper() {
    local helper="$1"
    # Try to run --version, if it fails (e.g., missing libraries) return false
    "$helper" --version &>/dev/null 2>&1
}

# Detect which AUR helper is available and working
get_aur_helper() {
    # Check yay first (more reliable on WSL)
    if command -v yay &>/dev/null && _test_aur_helper yay; then
        echo "yay"
        return 0
    fi

    # Then check paru
    if command -v paru &>/dev/null && _test_aur_helper paru; then
        echo "paru"
        return 0
    fi

    echo ""
}

check_dependencies() {
    print_header "Checking Dependencies"
    local deps=(git curl openssh fish)
    for dep in "${deps[@]}"; do
        if ! command -v "$dep" &>/dev/null; then
            echo "$dep not found - installing..."
            sudo pacman -S --noconfirm --needed "$dep"
        fi
    done
}

install_aur_helper() {
    local helper
    helper=$(get_aur_helper)

    if [[ -n "$helper" ]]; then
        echo -e "${GREEN}AUR helper already installed and working: $helper${NC}"
        return 0
    fi

    print_header "Installing AUR Helper"
    sudo pacman -S --noconfirm --needed base-devel git

    # Remove broken paru if it exists but doesn't work
    if command -v paru &>/dev/null && ! _test_aur_helper paru; then
        echo -e "${YELLOW}Removing broken paru installation...${NC}"
        sudo pacman -Rns --noconfirm paru 2>/dev/null || true
    fi

    # Try yay-bin first (pre-compiled, fewer build issues, better for WSL)
    echo "Installing yay-bin (pre-compiled)..."
    local temp_dir
    temp_dir=$(mktemp -d)
    if git clone https://aur.archlinux.org/yay-bin.git "$temp_dir" && \
       (cd "$temp_dir" && makepkg -si --noconfirm); then
        rm -rf "$temp_dir"
        echo -e "${GREEN}yay installed successfully${NC}"
        return 0
    fi
    rm -rf "$temp_dir"

    # Fallback to paru-bin
    echo "yay-bin failed, trying paru-bin..."
    temp_dir=$(mktemp -d)
    if git clone https://aur.archlinux.org/paru-bin.git "$temp_dir" && \
       (cd "$temp_dir" && makepkg -si --noconfirm); then
        rm -rf "$temp_dir"
        echo -e "${GREEN}paru installed successfully${NC}"
        return 0
    fi
    rm -rf "$temp_dir"

    echo -e "${RED}Failed to install AUR helper${NC}"
    return 1
}

# Alias for backwards compatibility
install_paru() {
    install_aur_helper
}

install_packages() {
    local pkg_type=$1
    print_header "Installing $pkg_type Packages"
    local pkg_file="$MIOZU_DIR/bin/dependencies/${pkg_type}-packages.txt"

    local aur_helper
    aur_helper=$(get_aur_helper)

    if [[ -z "$aur_helper" ]]; then
        echo -e "${RED}No AUR helper found. Run install_aur_helper first.${NC}"
        return 1
    fi

    echo "Using AUR helper: $aur_helper"

    # Filter out xmonad packages if using Stack
    local packages
    if [[ "$USE_STACK_XMONAD" == "true" ]] && [[ "$pkg_type" == "required" ]]; then
        echo "Filtering out XMonad packages (using Stack instead)..."
        packages=$(grep -v '^#' "$pkg_file" | grep -v '^$' | grep -v '^xmonad' | grep -v '^xmonad-contrib' | tr '\n' ' ')
    else
        packages=$(grep -v '^#' "$pkg_file" | grep -v '^$' | tr '\n' ' ')
    fi

    $aur_helper -S --needed --noconfirm $packages
}
