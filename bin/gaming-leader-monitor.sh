#!/bin/bash

echo "Starting gaming leader key monitor..."
echo "This will automatically fix your leader key every 10 seconds while gaming"
echo "Press Ctrl+C to stop"
echo ""

# Counter for less frequent messages
counter=0

while true; do
    # Check if BDO or other games are running
    if pgrep -f "BlackDesert\|steam" > /dev/null; then
        # Silently fix the leader key
        xdotool key --clearmodifiers XF86Ungrab 2>/dev/null || true
        xmodmap -e "clear mod4" 2>/dev/null
        xmodmap -e "add mod4 = Super_L Super_R" 2>/dev/null
        
        # Only print status every 60 seconds
        if [ $((counter % 6)) -eq 0 ]; then
            echo "$(date +%H:%M:%S) - Gaming detected, maintaining leader key..."
        fi
        counter=$((counter + 1))
    fi
    
    sleep 10
done