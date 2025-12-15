#!/bin/bash
# Package management functions for Miozu installer

# Detect which AUR helper is available
get_aur_helper() {
    if command -v paru &>/dev/null; then
        echo "paru"
    elif command -v yay &>/dev/null; then
        echo "yay"
    else
        echo ""
    fi
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
        echo "AUR helper already installed: $helper"
        return 0
    fi

    print_header "Installing AUR Helper"
    sudo pacman -S --noconfirm --needed base-devel git

    # Try yay-bin first (pre-compiled, fewer build issues)
    echo "Installing yay-bin..."
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

    # Filter out xmonad packages if using Stack
    if [[ "$USE_STACK_XMONAD" == "true" ]] && [[ "$pkg_type" == "required" ]]; then
        echo "Filtering out XMonad packages (using Stack instead)..."
        local packages=$(grep -v '^#' "$pkg_file" | grep -v '^xmonad' | grep -v '^xmonad-contrib' | tr '\n' ' ')
    else
        local packages=$(grep -v '^#' "$pkg_file" | tr '\n' ' ')
    fi

    $aur_helper -S --needed --noconfirm $packages
}