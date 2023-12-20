#!/bin/bash

# Get the name of the connected external monitor (other than eDP-1)
EXTERNAL_MONITOR=$(xrandr | grep -w "connected" | grep -v "eDP" | awk '{print $1}')
DECK=$(xrandr | grep -w "connected" | grep "eDP" | awk '{print $1}')

# Set the resolution and refresh rate for the external monitor
xrandr --output "$EXTERNAL_MONITOR" --mode 1920x1080 --primary --rate 120 --pos 0x0 --output "$DECK" --rotate right --pos 1920x0

# Set the primary monitor
#mons --primary $EXTERNAL_MONITOR
