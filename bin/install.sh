#!/bin/bash
# Last updated 12 May 2023
# Author: Nicholas Glazer <glazer.nicholas@gmail.com>
#!/bin/bash

# Miozu Environment Installer
set -euo pipefail
trap 'echo -e "${RED}Error at line $LINENO${NC}"; exit 1' ERR

# Configuration
MIOZU_DIR="$HOME/.miozu"
BACKUP_DIR="$HOME/.tmp/configs_backup"
LOG_FILE="$HOME/.miozu_install.log"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Initialize logging
exec > >(tee -a "$LOG_FILE") 2>&1

# --- Functions ---
print_header() {
    echo -e "${YELLOW}\n=== $1 ===${NC}"
}

check_dependencies() {
    print_header "Checking Dependencies"
    local deps=(git curl openssh)
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

setup_directories() {
    print_header "Creating Directory Structure"
    local dirs=(
        "$HOME/.ssh"
        "$HOME/.config/doom/themes"
        "$HOME/.config/wezterm/colors"
        "$HOME/code/github"
    )
    
    for dir in "${dirs[@]}"; do
        if [ ! -d "$dir" ]; then
            mkdir -p "$dir"
            echo "Created directory: $dir"
        fi
    done
    chmod 700 "$HOME/.ssh"
}

handle_miozu_repo() {
    print_header "Updating Miozu Repository"
    local repo_url="https://github.com/nicholasglazer/miozu.git"
    local retries=3
    local success=false

    for ((i=0; i<retries; i++)); do
        if [ -d "$MIOZU_DIR" ]; then
            echo "Found existing repository, updating..."
            (
                cd "$MIOZU_DIR"
                git remote set-url origin "$repo_url"
                git fetch --all
                if [ $(git rev-parse HEAD) != $(git rev-parse @{u}) ]; then
                    git reset --hard HEAD
                    git clean -fd
                    git pull --ff-only
                fi
            ) && success=true && break
        else
            echo "Cloning fresh repository..."
            git clone "$repo_url" "$MIOZU_DIR" && success=true && break
        fi
        echo "Retrying in 5 seconds..."
        sleep 5
    done

    if ! $success; then
        echo -e "${RED}Failed to update repository${NC}"
        exit 1
    fi
}

install_packages() {
    local pkg_type=$1
    print_header "Installing $pkg_type Packages"
    local pkg_file="$MIOZU_DIR/bin/dependencies/${pkg_type}-packages.txt"
    
    paru -S --needed --noconfirm $(grep -v '^#' "$pkg_file" | tr '\n' ' ')
}

setup_dotfiles() {
    print_header "Configuring Dotfiles"
    rsync -av --backup --backup-dir="$BACKUP_DIR" \
        "$MIOZU_DIR/" "$HOME/" \
        --exclude='.git/' \
        --exclude='.tmp/'
}

configure_services() {
    print_header "Configuring Services"
    sudo systemctl enable --now bluetooth.service
    systemctl --user enable --now emacs.service
}

setup_emacs() {
    print_header "Setting Up Emacs"
    if [ ! -d "$HOME/.emacs.d" ]; then
        git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
    fi
    ~/.emacs.d/bin/doom install
    
    # Apply custom theme
    if [ ! -d "$HOME/.config/doom/themes/miozu" ]; then
        git clone https://github.com/miozutheme/doom-miozu.git \
            "$HOME/.config/doom/themes/miozu"
    fi
}

post_install() {
    print_header "Finalizing Setup"
    chsh -s "$(which fish)"
    fc-cache -fv
    xmonad --recompile && xmonad --restart
    echo -e "${GREEN}\nInstallation complete!${NC}"
}

# --- Main Execution ---
main() {
    echo -e "${YELLOW}Starting Miozu Environment Installation${NC}"
    
    # Initial setup
    check_dependencies
    install_paru
    setup_directories
    handle_miozu_repo
    
    # Package installation
    install_packages "required"
    
    # Optional components
    read -p "Install optional packages? [y/N]: " -n 1 -r
    [[ $REPLY =~ ^[Yy]$ ]] && install_packages "optional"
    
    read -p "Install developer tools? [y/N]: " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        install_packages "developer"
        setup_emacs
    fi
    
    # Configuration
    setup_dotfiles
    configure_services
    post_install
}

# Keep sudo alive in background
while true; do
    sudo -n true
    sleep 60
    kill -0 "$$" || exit
done 2>/dev/null &

# Start main process
main
