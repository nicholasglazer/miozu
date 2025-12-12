#!/bin/bash

# Enable tap-to-click for Lenovo ThinkPad P1 Gen7 touchpad
# This script is run at X11 startup to ensure tap-to-click is always enabled

# Find touchpad device ID
TOUCHPAD_ID=$(xinput list | grep -i "SNSL002D:00 2C2F:002D Touchpad" | sed -n 's/.*id=\([0-9]\+\).*/\1/p')

if [ -n "$TOUCHPAD_ID" ]; then
    # Enable tap-to-click
    xinput set-prop "$TOUCHPAD_ID" "libinput Tapping Enabled" 1
    echo "Tap-to-click enabled for touchpad (ID: $TOUCHPAD_ID)"
    
    # Optional: Set other touchpad properties for better experience
    # Enable natural scrolling (optional - comment out if not desired)
    # xinput set-prop "$TOUCHPAD_ID" "libinput Natural Scrolling Enabled" 1
    
    # Set click method to clickfinger (optional - for better multitouch gestures)
    # xinput set-prop "$TOUCHPAD_ID" "libinput Click Method Enabled" 0 1
else
    echo "Touchpad not found!"
    exit 1
fi