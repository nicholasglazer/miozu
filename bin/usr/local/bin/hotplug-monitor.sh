#!/bin/sh
#
# File location: /usr/local/bin/hotplug-monitor.sh
# Description: Sends X display to an external monitor and turns internal
# display off when an HDMI cable is physically connected; turns monitor display
# off and internal display back on when HDMI cable is physically disconnected.

# Specify your username and user ID
USER_NAME=n  # find with `id -un` or `whoami`
USER_ID=1000 # find with `id -u`

# Export user's X-related environment variables
export DISPLAY=":0"
export XAUTHORITY="/home/${USER_NAME}/.Xauthority"
export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/${USER_ID}/bus"

# Video output and device names recogized by xrandr/sysfs
internal="eDP-1"    # change as needed
external="DP-2"     # change as needed
device="card1-DP-2" # change as needed

# If external display was just physically connected, turn external display on
# and (optionally) turn internal display off to save battery.
if [ $(cat /sys/class/drm/${device}/status) == "connected" ]; then
    xrandr --output "${external}" --auto # sends display to monitor
    xrandr --output "${internal}" --off  # optionally turn internal display off

    # If external display was just physically disconnected, turn
    # external display off and turn internal display on.
elif [ $(cat /sys/class/drm/${device}/status) == "disconnected" ]; then
    xrandr --output "${external}" --off  # turn monitor display off
    xrandr --output "${internal}" --auto # turn internal display on (if needed)
else                                     # Do nothing if device status is unreadable
    exit
fi
