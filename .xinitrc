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

# Check if there are more than one connected monitors 
# (comment this and use arandr if you need more than one monitor)
if [ $(xrandr | grep -w "connected" | wc -l) -gt 1 ]; then
  # Turn off the first connected monitor
  xrandr --output $(xrandr | grep -w "connected" | awk '{print $1}' | sed -n 1p) --off
fi

# Set up the Xresources
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources &

# Make correct cursor and make background solid with hex color
xsetroot -cursor_name left_ptr &
hsetroot -solid '#252835' &

# Uncomment to use image instaed of solid hex background color
feh --bg-fill $HOME/Pictures/wallpapers/miozu_keybindings.png &

# Start compositor
DISPLAY=":0" picom -b


# Keyboard layout
setxkbmap -layout "dvorak,us" -option "grp:alt_shift_toggle,caps:escape" &

# Start WM
exec xmonad
