#!/bin/bash

brightness_file="$HOME/.tmp/brightness.txt"

if [ -f "$brightness_file" ]; then
    last_brightness=$(cat "$brightness_file")
else
    last_brightness=$(brightnessctl g) # Get current brightness if the file doesn't exist
fi

current_brightness=$(brightnessctl g)

if [ "$current_brightness" != "0" ]; then
    brightnessctl s 0
    echo "set to 0" >"$brightness_file"
else
    brightnessctl s "$last_brightness"
    echo "set to $last_brightness" >"$brightness_file"
fi
