#!/bin/sh
#
# .xinitrc
#
# Executed by startx

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Set up the resources
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources &

#Use not "X" for the cursor
xsetroot -cursor_name left_ptr &

# Load compton
compton -b &

# Keyboard layout
setxkbmap -layout "dvorak,ru" -option "grp:alt_shift_toggle,caps:escape" &

#Wallpaper
feh  --bg-scale '/home/me/images/cosmos-paint.png' &

# Start WM
#exec awesome
exec xmonad
