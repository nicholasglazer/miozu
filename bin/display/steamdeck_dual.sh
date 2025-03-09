#!/bin/bash
# TODO make varibales like rate global . conf

# Might help to figure out if "xrandr" is not helpful: "ls /sys/class/drm"
INTERNAL_NAME="eDP"

# Get the name of the connected external monitor (other than the internal display)
EXTERNAL_MONITOR=$(xrandr | grep -w "connected" | grep -v "$INTERNAL_NAME" | awk '{print $1}')
INTERNAL_MONITOR=$(xrandr | grep -w "connected" | grep "$INTERNAL_NAME" | awk '{print $1}')

# Set the resolution and refresh rate for the external monitor
xrandr --output "$EXTERNAL_MONITOR" --mode 1920x1080 --primary --rate 60 --pos 0x0 --output "$INTERNAL_MONITOR" --rotate right --pos 1921x0

# Check the exit status of the last command
if [ $? -ne 0 ]; then
  # If the last command failed, execute xrandr --auto
  xrandr --auto
fi
