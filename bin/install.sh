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

# Initialize logging
exec > >(tee -a "$LOG_FILE") 2>&1

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source all library modules
source "$SCRIPT_DIR/lib/common.sh"
source "$SCRIPT_DIR/lib/packages.sh"
source "$SCRIPT_DIR/lib/config.sh"
source "$SCRIPT_DIR/lib/services.sh"
source "$SCRIPT_DIR/lib/keyboard.sh"


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
    echo -n "Install optional packages? [y/N]: "
    read -n 1 -r REPLY < /dev/tty
    echo
    [[ $REPLY =~ ^[Yy]$ ]] && install_packages "optional"

    echo -n "Install developer tools? [y/N]: "
    read -n 1 -r REPLY < /dev/tty
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        install_packages "developer"
        setup_emacs
    fi

    # Configuration
    setup_dotfiles
    configure_services
    
    # Keyboard setup at the end to avoid password issues
    setup_keyboard
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
