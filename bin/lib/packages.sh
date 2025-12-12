#!/bin/bash
# Package management functions for Miozu installer

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

install_paru() {
    if ! command -v paru &>/dev/null; then
        print_header "Installing Paru"
        sudo pacman -S --noconfirm --needed base-devel
        temp_dir=$(mktemp -d)
        git clone https://aur.archlinux.org/paru.git "$temp_dir"
        (cd "$temp_dir" && makepkg -si --noconfirm)
        rm -rf "$temp_dir"
    fi
}

install_packages() {
    local pkg_type=$1
    print_header "Installing $pkg_type Packages"
    local pkg_file="$MIOZU_DIR/bin/dependencies/${pkg_type}-packages.txt"
    
    # Filter out xmonad packages if using Stack
    if [[ "$USE_STACK_XMONAD" == "true" ]] && [[ "$pkg_type" == "required" ]]; then
        echo "Filtering out XMonad packages (using Stack instead)..."
        local packages=$(grep -v '^#' "$pkg_file" | grep -v '^xmonad' | grep -v '^xmonad-contrib' | tr '\n' ' ')
    else
        local packages=$(grep -v '^#' "$pkg_file" | tr '\n' ' ')
    fi

    paru -S --needed --noconfirm $packages
}