#!/bin/bash

echo "ULTRA aggressive fix - 5 times per second!"

# Run 5 times per second
while true; do
    # Method 1: Force ungrab
    xdotool key XF86Ungrab 2>/dev/null
    
    # Method 2: Just re-add Super to mod4 without clearing
    # This is faster than clear + add
    xmodmap -e "add mod4 = Super_L Super_R" 2>/dev/null
    
    # 200ms delay = 5 times per second
    sleep 0.2
done