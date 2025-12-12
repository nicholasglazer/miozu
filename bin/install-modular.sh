#!/bin/bash
# Modular Miozu Environment Installer
# Author: Nicholas Glazer <glazer.nicholas@gmail.com>

set -euo pipefail
trap 'echo -e "${RED}Error at line $LINENO${NC}"; exit 1' ERR

# Configuration
MIOZU_DIR="$HOME/.miozu"
BACKUP_DIR="$HOME/.tmp/configs_backup"
LOG_FILE="$HOME/.miozu_install.log"

# Installation options
USE_STACK_XMONAD=${USE_STACK_XMONAD:-true}  # Set to false to use AUR xmonad

# Initialize logging
exec > >(tee -a "$LOG_FILE") 2>&1

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source all library modules
echo "Loading installation modules..."
source "$SCRIPT_DIR/lib/common.sh"
source "$SCRIPT_DIR/lib/packages.sh"
source "$SCRIPT_DIR/lib/config.sh"
source "$SCRIPT_DIR/lib/services.sh"
source "$SCRIPT_DIR/lib/keyboard.sh"
source "$SCRIPT_DIR/lib/stack.sh"
source "$SCRIPT_DIR/lib/xmonad-stack.sh"

# --- Installation Functions ---

install_base_system() {
    print_header "Base System Installation"
    check_dependencies
    install_paru
    setup_directories
    handle_miozu_repo
    setup_monitor_hotplug
}

install_xmonad_environment() {
    print_header "XMonad Environment Installation"
    
    if [[ "$USE_STACK_XMONAD" == "true" ]]; then
        echo -e "${YELLOW}Installing XMonad via Stack (recommended)${NC}"
        
        # Install Stack
        install_stack || {
            echo -e "${RED}Stack installation failed${NC}"
            return 1
        }
        
        # Setup Stack for Haskell development
        setup_stack_xmonad || {
            echo -e "${RED}Stack XMonad setup failed${NC}"
            return 1
        }
        
        # Setup XMonad with Stack
        setup_xmonad_stack || {
            echo -e "${RED}XMonad Stack setup failed${NC}"
            return 1
        }
        
        # Build XMonad
        build_xmonad_stack || {
            echo -e "${RED}XMonad build failed${NC}"
            return 1
        }
        
        # Verify installation
        verify_stack_setup
        verify_xmonad_stack
    else
        echo -e "${YELLOW}Installing XMonad via AUR${NC}"
        # AUR packages will be installed with other required packages
    fi
}

install_optional_components() {
    print_header "Optional Components"
    
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
}

configure_system() {
    print_header "System Configuration"
    setup_dotfiles
    configure_services
    
    # Compile XMonad configuration if using Stack
    if [[ "$USE_STACK_XMONAD" == "true" ]]; then
        compile_xmonad_config || {
            echo -e "${YELLOW}XMonad configuration compilation failed - check your config${NC}"
        }
    fi
    
    # Keyboard setup at the end to avoid password issues
    setup_keyboard
    post_install
}

# --- Main Menu ---
show_menu() {
    clear
    echo -e "${YELLOW}=== Miozu Environment Installer ===${NC}"
    echo
    echo "Installation Options:"
    echo "1) Full installation (recommended)"
    echo "2) Base system only"
    echo "3) XMonad environment only"
    echo "4) Optional components only"
    echo "5) Repair/Verify installation"
    echo "6) Clean and reinstall"
    echo "q) Quit"
    echo
    echo -n "Select option: "
}

# --- Main Execution ---
main() {
    while true; do
        show_menu
        read -n 1 -r choice < /dev/tty
        echo
        
        case $choice in
            1)
                echo -e "${YELLOW}Starting full Miozu installation...${NC}"
                install_base_system
                install_packages "required"
                install_xmonad_environment
                install_optional_components
                configure_system
                echo -e "${GREEN}Full installation complete!${NC}"
                break
                ;;
            2)
                echo -e "${YELLOW}Installing base system...${NC}"
                install_base_system
                install_packages "required"
                echo -e "${GREEN}Base system installation complete!${NC}"
                ;;
            3)
                echo -e "${YELLOW}Installing XMonad environment...${NC}"
                install_xmonad_environment
                if [[ "$USE_STACK_XMONAD" == "true" ]]; then
                    compile_xmonad_config
                fi
                echo -e "${GREEN}XMonad installation complete!${NC}"
                ;;
            4)
                echo -e "${YELLOW}Installing optional components...${NC}"
                install_optional_components
                echo -e "${GREEN}Optional components installed!${NC}"
                ;;
            5)
                echo -e "${YELLOW}Verifying installation...${NC}"
                if [[ "$USE_STACK_XMONAD" == "true" ]]; then
                    verify_stack_setup
                    verify_xmonad_stack
                fi
                echo -e "${GREEN}Verification complete!${NC}"
                ;;
            6)
                echo -e "${YELLOW}Clean reinstall...${NC}"
                echo -n "This will remove XMonad build artifacts. Continue? [y/N]: "
                read -n 1 -r REPLY < /dev/tty
                echo
                if [[ $REPLY =~ ^[Yy]$ ]]; then
                    clean_xmonad_build
                    main
                fi
                ;;
            q|Q)
                echo "Exiting installer..."
                exit 0
                ;;
            *)
                echo -e "${RED}Invalid option${NC}"
                sleep 1
                ;;
        esac
        
        if [[ "$choice" != "1" ]]; then
            echo
            echo -n "Press any key to continue..."
            read -n 1 -r < /dev/tty
        fi
    done
}

# Keep sudo alive in background
while true; do
    sudo -n true
    sleep 60
    kill -0 "$$" || exit
done 2>/dev/null &

# Start main process
echo -e "${YELLOW}Miozu Modular Installer${NC}"
echo "Using Stack for XMonad: $USE_STACK_XMONAD"
echo
main