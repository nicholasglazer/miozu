#!/bin/bash

echo "Monitoring for Super key grabs..."
echo "When leader key stops working, check the output!"
echo ""

# Function to check if Super key works
check_super() {
    timeout 0.5 xinput test-xi2 --root 2>/dev/null | grep -q "133\|134\|115\|116" && echo "✓" || echo "✗"
}

# Initial ungrab
/home/ng/.miozu/bin/force-ungrab-keys.sh > /dev/null 2>&1

echo "Starting monitoring loop..."
echo "Time | Super Works | Active Window"
echo "--------------------------------"

LAST_STATE="working"
while true; do
    # Check if Super key is responsive
    SUPER_STATUS=$(check_super)
    
    # Get active window
    ACTIVE_WIN=$(xdotool getactivewindow getwindowname 2>/dev/null || echo "unknown")
    ACTIVE_PID=$(xdotool getactivewindow getwindowpid 2>/dev/null || echo "?")
    
    # Print status
    echo "$(date +%H:%M:%S) | $SUPER_STATUS | $ACTIVE_WIN (PID: $ACTIVE_PID)"
    
    # If Super stopped working, capture more info
    if [ "$SUPER_STATUS" = "✗" ] && [ "$LAST_STATE" = "working" ]; then
        echo "!!! SUPER KEY GRABBED !!!"
        echo "Processes that might be responsible:"
        ps aux | grep -E "(steam|game|overlay|input)" | grep -v grep | awk '{print $2, $11}' | head -10
        echo "---"
        LAST_STATE="broken"
    elif [ "$SUPER_STATUS" = "✓" ] && [ "$LAST_STATE" = "broken" ]; then
        echo "!!! SUPER KEY RELEASED !!!"
        LAST_STATE="working"
    fi
    
    sleep 2
done