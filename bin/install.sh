#!/bin/bash
# Last updated 12 May 2023
# Author: Nicholas Glazer <glazer.nicholas@gmail.com>
#
# Miozu dotfiles package varibales
MIOZU_DIR="$HOME/.miozu"
#MIOZU_REPO_URL="https://github.com/nicholasglazer/miozu.git"

# Set password once
# TODO FIX password should be prompted only once
read -r -s -p "Hey, you're about to install miozu environment. Enter your password: " password

# Create all necessary directories in a loop
dirs=(
	"$HOME/.miozu"
	"$HOME/.ssh"
	"$HOME/.config/aria2"
	"$HOME/.config/doom/themes"
	"$HOME/.config/dunst"
	"$HOME/.config/fish"
	"$HOME/.config/fontconfig"
	"$HOME/.config/nvim"
	"$HOME/.config/rofi/colors"
	"$HOME/.config/wezterm/colors"
	"$HOME/.config/xmonad/xmobar"
	"$HOME/.config/xmonad/lib"
	"$HOME/.config/zathura"
	"$HOME/.tmp/configs_backup"
	"$HOME/code/github"
	"$HOME/code/sandbox"
	"$HOME/Documents"
	"$HOME/Downloads"
	"$HOME/Music"
	"$HOME/Pictures/gifs"
	"$HOME/Pictures/screenshots"
	"$HOME/Videos"
)

# if dir doesn't exist, create one
for dir in "${dirs[@]}"; do
	if [ ! -d "$dir" ]; then
		mkdir -p "$dir"
	fi
done

# Check if dir exists and it is a git repo
if [ -d "$MIOZU_DIR" ] && [ -d "$MIOZU_DIR/.git" ]; then
	cd "$MIOZU_DIR"
	# Fetch latest changes
	git fetch
	if [ $(git rev-parse HEAD) != $(git rev-parse @{u}) ]; then
		git pull
		echo "Miozu repository updated"
	else
		echo "Miozu repository up to date"
	fi
else
	echo "Miozu repository not found at '$MIOZU_DIR'"
	exit 1
fi

# Install req packages
echo "$password" | paru -Syu --noconfirm --needed $(grep -v '^#' $MIOZU_DIR/bin/dependencies/required-packages.txt)

# Prompt to install optional packages: browser and some social tools
read -r -p "Do you want to install optional packages? (y/n): " choice
if [[ $choice =~ ^[yY] ]]; then
	# install optional packages with paru
	paru -S --noconfirm --needed $(grep -v '^#' $MIOZU_DIR/bin/dependencies/optional-packages.txt)
fi

# This will install Emacs IDE as well as its dependencies and other tools and languages
# To keep it simple, I removed many packages like python, rust etc (maybe I'll add them in the future)
read -r -p "Do you want to install developer packages? (y/n): " choice
if [[ $choice =~ ^[yY] ]]; then
	# install optional packages with paru
	paru -S --noconfirm --needed $(grep -v '^#' $MIOZU_DIR/bin/dependencies/developer-packages.txt)
	# install doom-emacs
	mv $HOME/.emacs.d $HOME/.tmp/.emacs.d.bak
	git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
	~/.emacs.d/bin/doom install
	# install miozu theme for doom-emacs
	git clone https://github.com/miozutheme/doom-miozu.git $HOME/.config/doom/themes
	# run emacs as a systemd unit
	systemctl --user enable emacs.service
	systemctl --user start emacs.service
fi

declare -A dotfiles=(
	[".config/aria2"]="$HOME/.config/aria2"
	[".config/doom"]="$HOME/.config/doom"
	[".config/dunst"]="$HOME/.config/dunst"
	[".config/fish"]="$HOME/.config/fish"
	[".config/fontconfig"]="$HOME/.config/fontconfig"
	[".config/nvim"]="$HOME/.config/nvim"
	[".config/rofi"]="$HOME/.config/rofi"
	[".config/wezterm"]="$HOME/.config/wezterm"
	[".config/xmonad/xmobar"]="$HOME/.config/xmonad/xmobar"
	[".config/xmonad/lib"]="$HOME/.config/xmonad/lib"
	[".config/xmonad"]="$HOME/.config/xmonad"
	[".config/zathura"]="$HOME/.config/zathura"
	[".xinitrc"]="$HOME/.xinitrc"
	[".Xresources"]="$HOME/.Xresources"
	[".gitconfig"]="$HOME/.gitconfig"
	["X11/xorg.conf.d/00-keyboard.conf"]="/etc/X11/xorg.conf.d/00-keyboard.conf"
	["X11/xorg.conf.d/30-touchpad.conf"]="/etc/X11/xorg.conf.d/30-touchpad.conf"
)

# Create a backup dir for already existing configs
backup_dir="$HOME/.tmp/configs_backup"

for file in "${!dotfiles[@]}"; do
	dest="${dotfiles[$file]}"
	backup="$backup_dir/$file.$(date +%Y%m%d_%H:%M.bkp)"
	# check if file exists and it's not a symbolic link
	if [ -e "$dest" ] && [ ! -L "$dest" ]; then
		echo "Backing up $dest to $backup"
		echo "$password" | mv "$dest" "$backup"
	fi
	# check if file is a directory
	if [ -d "$MIOZU_DIR/$file" ]; then
		# if yes, link all it contents to $dest
		for f in "$MIOZU_DIR/$file"/*; do
			ln -sf "$f" "$dest/$(basename "$f")"
			echo "Linked $f to $dest/$(basename "$f")"
		done
	else
		# if not, simply link the file
		ln -sf "$MIOZU_DIR/$file" "$dest"
		echo "Linked $MIOZU_DIR/$file to $dest"
	fi
done

# Clone themes:
# wezterm
git clone https://github.com/miozutheme/wezterm.git $HOME/.config/wezterm/colors
# rofi
git clone https://github.com/miozutheme/rofi.git $HOME/.config/rofi/colors
# xmonad
git clone https://github.com/miozutheme/xmonad.git $HOME/.config/xmonad/lib/Themes

# Find the location of the Fish shell executable with 'which'. 'chsh' to use Fish as a default shell
chsh -s $(which fish)
echo "Fish is now your default shell!"
# Write path for miozu dir into fish config to make a global variable
sed -i 's|# INSERT MIOZU_DIR HERE|set -x MIOZU_DIR '"$MIOZU_DIR"'|' $MIOZU_DIR/.config/fish/config.fish
# TODO For later usage to update repo | so you can write something like "miozu update"
#sed -i 's|# INSERT MIOZU GLOBAL|set -x MIOZU_DIR '"$MIOZU_DIR"'|' $MIOZU_DIR/.config/fish/config.fish

# Enable & start services/daemons
# Bluetooth
echo "$password" | sudo systemctl enable bluetooth.service
echo "$password" | sudo systemctl start bluetooth.service
# Set the AutoEnable option in the Bluetooth service configuration to automatically pair devices with 10 attempts each 3 sec
sudo sed -i 's/#AutoEnable=.*/AutoEnable=true/' /etc/bluetooth/main.conf
sudo sed -i 's/#ReconnectAttempts=.*/ReconnectAttempts=10/' /etc/bluetooth/main.conf
sudo sed -i 's/#ReconnectIntervals=.*/ReconnectIntervals=3/' /etc/bluetooth/main.conf
# Greenclip TODO implement rofi-greenclip later
#systemctl --user enable greenclip.service
#systemctl --user start greenclip.service

# set syttem clock according to api
timedatectl set-timezone "$(curl --fail https://ipapi.co/timezone)"

# Create a /etc/X11/xorg.conf.d/00-keyboard.conf to persist languages and layouts
# NOTE ukrainian language and us dvorak layout, change this line to match your preferences
localectl set-x11-keymap us,ua "" dvorak grp:alt_shift_toggle,caps:escape


# Enable tor
echo "$password" | sudo systemctl enable tor.service

# Recompile and restart xmonad
xmonad --recompile && xmonad --restart
