#!/bin/bash

echo "Starting AGGRESSIVE leader key fix - every 1 second"
echo "This will run until you press Ctrl+C"
echo ""

# Run in background with minimal output
while true; do
    # Multiple ungrab methods
    xdotool key XF86Ungrab 2>/dev/null
    xdotool key --clearmodifiers XF86Ungrab 2>/dev/null
    
    # Force release stuck keys
    xdotool keyup Super_L keyup Super_R 2>/dev/null
    
    # Quick mod4 reset without full keyboard reset
    xmodmap -e "clear mod4" 2>/dev/null
    xmodmap -e "add mod4 = Super_L Super_R" 2>/dev/null
    
    # No output unless you want to debug
    # echo -n "."
    
    sleep 1
done