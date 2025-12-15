#!/bin/bash
# Configuration functions for Miozu installer

# Check if running in WSL (can be overridden by sourcing wsl.sh)
_is_wsl() {
    grep -qi microsoft /proc/version 2>/dev/null
}

setup_directories() {
    print_header "Creating Directory Structure"
    local dirs=(
        "$HOME/.ssh"
        "$HOME/.config/doom/themes"
        "$HOME/.config/wezterm/colors"
        "$HOME/code/github"
        "$HOME/Videos/gifs"
        "$HOME/Videos/recordings"
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

    # Skip in WSL - no hardware monitor hotplug
    if _is_wsl; then
        echo "Skipping monitor hotplug setup (not applicable in WSL)"
        return 0
    fi

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

    # Set default shell to fish
    if [[ "$SHELL" != "$(which fish)" ]]; then
        chsh -s "$(which fish)" || echo -e "${YELLOW}Could not change shell, run: chsh -s $(which fish)${NC}"
    fi

    # Rebuild font cache
    fc-cache -fv

    # Compile XMonad (skip restart in WSL if no X display)
    if _is_wsl; then
        echo "Compiling XMonad configuration..."
        xmonad --recompile || echo -e "${YELLOW}XMonad compilation failed - check config${NC}"
        echo -e "${GREEN}\nWSL Installation complete!${NC}"
        echo "Run 'start-xmonad' to launch XMonad (after setting up VcXsrv/WSLg)"
    else
        xmonad --recompile && xmonad --restart
        echo -e "${GREEN}\nInstallation complete!${NC}"
    fi
}