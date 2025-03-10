#!/bin/bash
# Last updated 9 March 2025
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

    for ((i = 0; i < retries; i++)); do
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

    # make configs backup
    mkdir -p "$BACKUP_DIR"

    # Git itself for dotfile management
    (
        cd "$MIOZU_DIR"
        git ls-files | while read -r file; do
            src_file="$MIOZU_DIR/$file"
            dest_file="$HOME/$file"

            # Backup existing files
            if [ -f "$dest_file" ]; then
                mkdir -p "$BACKUP_DIR/$(dirname "$file")"
                mv -v "$dest_file" "$BACKUP_DIR/$file"
            fi

            # Create parent directory if needed
            mkdir -p "$(dirname "$dest_file")"

            # Create symlink
            ln -sfv "$src_file" "$dest_file"
        done
    )

    echo -e "${GREEN}Dotfiles configured with symlinks${NC}"
}

setup_monitor_hotplug() {
    print_header "Configuring Display Hotplug Autodetection"

    local script_src="$MIOZU_DIR/bin/display/monitor-hotplug.sh"
    local script_dest="/usr/local/bin/monitor-hotplug.sh"
    local udev_rule="/etc/udev/rules.d/95-monitor-hotplug.rules"
    local service_file="/etc/systemd/system/monitor-hotplug.service"

    # Validate script exists in repo
    if [ ! -f "$script_src" ]; then
        echo -e "${RED}Error: monitor-hotplug.sh not found in Miozu repository${NC}"
        return 1
    fi

    # Install script
    echo "Installing hotplug script to system bin"
    sudo cp -v "$script_src" "$script_dest"
    sudo chmod +x "$script_dest"

    # Create udev rule (updated service name)
    echo "Creating udev rule for display changes"
    sudo tee "$udev_rule" >/dev/null <<EOF
ACTION=="change", SUBSYSTEM=="drm", RUN+="/usr/bin/systemctl start monitor-hotplug.service"
EOF

    # Create systemd service (updated service name)
    echo "Creating hotplug service for user $USER"
    sudo tee "$service_file" >/dev/null <<EOF
[Unit]
Description=Monitor Hotplug Service
After=graphical.target

[Service]
Type=oneshot
User=$USER
Environment="DISPLAY=:0"
Environment="XAUTHORITY=/home/$USER/.Xauthority"
ExecStart=$script_dest

[Install]
WantedBy=multi-user.target
EOF

    # Enable system components
    sudo systemctl daemon-reload
    sudo systemctl enable monitor-hotplug.service
    sudo udevadm control --reload-rules
    sudo udevadm trigger

    echo -e "${GREEN}Hotplug configuration complete${NC}"
}

configure_services() {
    print_header "Configuring Services"
    sudo systemctl enable --now bluetooth.service

    # Configure emacs service more robustly
    if ! systemctl --user is-enabled emacs.service &>/dev/null; then
        echo "Enabling emacs service (will start at next login)"
        systemctl --user enable emacs.service
    fi

    # Try starting but don't fail installation
    echo "Attempting to start emacs service..."
    systemctl --user start emacs.service || true
}

setup_keyboard() {
    print_header "Keyboard Layout Setup"
    read -p "Choose keyboard layout (1: dvorak, 2: qwerty) [1/2]: " -n 1 -r
    echo

    if [[ $REPLY == "1" ]]; then
        echo "Setting up Dvorak layout..."
        sudo localectl set-keymap dvorak
        sudo localectl set-x11-keymap us pc105 dvorak

        # Symlink the Dvorak config
        if [ -f "$MIOZU_DIR/X11/xorg.conf.d/00-keyboard.conf" ]; then
            echo "Symlinking Dvorak configuration..."
            sudo ln -sf "$MIOZU_DIR/X11/xorg.conf.d/00-keyboard.conf" /etc/X11/xorg.conf.d/00-keyboard.conf
        else
            echo -e "${RED}Dvorak config not found!${NC}"
        fi
    else
        echo "Keeping QWERTY layout. No symlink created."
    fi
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
    setup_monitor_hotplug

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
    setup_keyboard
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
