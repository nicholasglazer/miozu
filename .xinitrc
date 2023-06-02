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

# Uncomment to use image instaed of solid hex background color
feh --bg-fill $HOME/Pictures/wallpapers/miozu_keybindings.png &

# Start notification manager
dunst &

# Run autorandr
autorandr -c

# Start compositor
DISPLAY=":0" picom -b

# Keyboard layout
setxkbmap -layout "dvorak,ua,ru,us" -option "grp:alt_shift_toggle,caps:escape" &

# Start WM
exec xmonad
