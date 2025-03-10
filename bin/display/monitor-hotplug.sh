#!/bin/sh
#
# Nicholas Glazer <info@nicgl.com>
#
## /.miozu/bin/hdmi_hotplug.sh
# Determine the active user
ACTIVE_USER=$(who | awk '{print $1}' | head -n 1)
# Set environment variables
export DISPLAY=:0
export XAUTHORITY=$(find /home/$ACTIVE_USER -maxdepth 1 -name '.Xauthority' -print -quit)
export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id -u $ACTIVE_USER)/bus
# TODO maybe to check this states too 'open' or 'closed'
# LID_STATE=$(awk -F': ' '{print $2}' /proc/acpi/button/lid/LID/state)

# Get all physically connected outputs
CONNECTED_OUTPUTS=$(xrandr | grep ' connected' | awk '{print $1}')

# Identify internal and external displays
INTERNAL_DISPLAY=$(echo "$CONNECTED_OUTPUTS" | grep -E '^eDP|^LVDS')
EXTERNAL_DISPLAY=$(echo "$CONNECTED_OUTPUTS" | grep -E '^HDMI|^DP')

# Functions to handle display configuration
switch_to_external() {
    xrandr --output "$INTERNAL_DISPLAY" --off --output "$EXTERNAL_DISPLAY" --primary --mode 1920x1080 --pos 0x0 --rotate normal
    echo "External display active: $EXTERNAL_DISPLAY"
}

switch_to_internal() {
    xrandr --output "$INTERNAL_DISPLAY" --primary --auto --output "$EXTERNAL_DISPLAY" --off
    echo "Internal display active: $INTERNAL_DISPLAY"
}

# Main logic
if [ -n "$EXTERNAL_DISPLAY" ]; then
    switch_to_external
elif [ -n "$INTERNAL_DISPLAY" ]; then
    switch_to_internal
else
    echo "No displays detected!"
fi
