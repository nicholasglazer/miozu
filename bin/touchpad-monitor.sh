#!/bin/bash

# Monitor touchpad issues and restart driver if necessary
# Run this script to diagnose intermittent touchpad failures

echo "Monitoring touchpad for issues..."
echo "Press Ctrl+C to stop"

TOUCHPAD_NAME="SNSL002D:00 2C2F:002D"
LOG_FILE="/tmp/touchpad-monitor.log"

# Clear previous log
> "$LOG_FILE"

check_touchpad() {
    # Check if touchpad is present in xinput
    if ! xinput list | grep -q "$TOUCHPAD_NAME"; then
        echo "$(date): Touchpad not found in xinput!" | tee -a "$LOG_FILE"
        return 1
    fi
    
    # Check i2c_hid module
    if ! lsmod | grep -q i2c_hid; then
        echo "$(date): i2c_hid module not loaded!" | tee -a "$LOG_FILE"
        return 1
    fi
    
    return 0
}

restart_touchpad() {
    echo "$(date): Attempting to restart touchpad..." | tee -a "$LOG_FILE"
    
    # Reload i2c_hid modules
    sudo modprobe -r i2c_hid_acpi i2c_hid 2>/dev/null
    sleep 1
    sudo modprobe i2c_hid_acpi
    
    # Wait for device to reappear
    sleep 2
    
    # Re-enable tap-to-click
    bash "$MIOZU_DIR/bin/touchpad-fix.sh"
}

# Initial check
if ! check_touchpad; then
    restart_touchpad
fi

# Monitor loop
while true; do
    if ! check_touchpad; then
        restart_touchpad
    fi
    
    # Check dmesg for recent i2c errors
    if sudo dmesg | tail -50 | grep -q "i2c_hid.*error\|i2c_hid.*timeout"; then
        echo "$(date): I2C errors detected in dmesg" | tee -a "$LOG_FILE"
        restart_touchpad
    fi
    
    sleep 5
done