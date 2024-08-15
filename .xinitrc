#!/bin/sh
# ****
# .xinitrc for Miozu by @nicholasglazer
# ****
# Check if the /etc/X11/xinit/xinitrc.d directory exists, and if it does, it executes any executable files within that directory
if [ -d /etc/X11/xinit/xinitrc.d ]; then
	for f in /etc/X11/xinit/xinitrc.d/*; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

# Set up the Xresources
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources &

# Make correct cursor and make background solid with hex color
xsetroot -cursor_name left_ptr &
hsetroot -solid '#252835' &

# nohup mons -a > /dev/null 2>&1 &
# mons -a -x "$MIOZU_DIR/.config/mons/dual_deck.sh"

# Uncomment to use image instaed of solid hex background color
feh --bg-fill $HOME/Pictures/wallpapers/miozu_keybindings.png &

# Start notification manager
dunst &

# Run dual monitor script
bash $MIOZU_DIR/bin/tools/deck_dual.sh

# Start compositor
#DISPLAY=":0" picom -b

# Keyboard layout
# set xorg 00-keyboard.conf every time
localectl set-x11-keymap us,ua , dvorak grp:alt_shift_toggle,caps:escape
# set xkb
setxkbmap -layout "us,ua" -variant "dvorak," -option "grp:alt_shift_toggle,caps:escape" &

# Start WM
exec xmonad
