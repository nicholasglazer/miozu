# --- Executables
# This command ensures that /usr/local/bin is searched first when looking for executable programs
set -x PATH /usr/local/bin $PATH
# Make doom-emacs as a var
set -x PATH $HOME/.emacs.d/bin $PATH
# Set an editor
set -x EDITOR "nvim"
# Comments below will be substituted with miozu variables automatically after you run ./install.sh
# INSERT MIOZU_DIR HERE
# INSERT MIOZU GLOBAL

# --- Internal settings
# Set fish-specific variables
# Remove fish greeting -U will need to run it once to remove completely
set -x fish_greeting ""
# set -U fish_greeting

# --- Aliases
# Please allow me ;)
alias pls 'sudo'
# List files with details and color
alias l 'ls -alhF'
# Shortcut for systemctl
alias s 'systemctl'
# Enable colored output for grep
alias grep 'grep --color=auto'
# Removing dir but preserving /root from deleting
alias rmf 'rm –preserve-root -Irfv'
# Navigate up one directory
alias .. 'cd ..'
# Violent way to close an app
alias k 'killall -9'
# Clear the terminal screen
alias c 'clear'
# Create a new file
alias t 'touch'
# Create a new directory and its parent directories if necessary
alias md 'mkdir -p'
# Ping the Arch Linux website with 3 packets
alias ping 'ping -c 3 https://archlinux.org'
# Open files in Neovim
alias vi 'nvim'
# Show all running services for the whole system
alias srs 'sudo systemctl list-units --type=service --state=running'
# Show all failed services for the whole system
alias sfs 'sudo systemctl --failed'
# Show all running services for the user
alias urs 'systemctl --user list-units --type=service --state=running'
# Show all failed services for the user
alias ufs 'systemctl --user --failed'
# Recompile xmonad
alias xr 'xmonad --recompile'
# Git clone
alias gc 'git clone'
# Run htop
alias h 'htop'
# Make a symlink
alias lns 'ln -si'
# Remove package
alias remove 'sudo pacman -Rsc'
