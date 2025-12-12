#!/bin/bash

# Battery monitoring script with notifications
# Sends notifications at critical battery levels

BATTERY_PATH="/sys/class/power_supply/BAT0"
CRITICAL_LEVEL=10
LOW_LEVEL=20
NOTIFICATION_SENT_FILE="/tmp/battery_notification_sent"

# Check if battery exists
if [ ! -d "$BATTERY_PATH" ]; then
    echo "Battery not found at $BATTERY_PATH"
    exit 1
fi

# Get battery information
CAPACITY=$(cat "$BATTERY_PATH/capacity" 2>/dev/null || echo "0")
STATUS=$(cat "$BATTERY_PATH/status" 2>/dev/null || echo "Unknown")

# Only send notifications when discharging
if [ "$STATUS" = "Discharging" ]; then
    if [ "$CAPACITY" -le "$CRITICAL_LEVEL" ]; then
        # Critical level notification
        if [ ! -f "$NOTIFICATION_SENT_FILE" ] || [ "$(cat "$NOTIFICATION_SENT_FILE")" != "critical" ]; then
            notify-send -u critical -i battery-caution \
                "Critical Battery Warning!" \
                "Battery at ${CAPACITY}%. System will shutdown soon!"
            echo "critical" > "$NOTIFICATION_SENT_FILE"
        fi
    elif [ "$CAPACITY" -le "$LOW_LEVEL" ]; then
        # Low level notification
        if [ ! -f "$NOTIFICATION_SENT_FILE" ] || [ "$(cat "$NOTIFICATION_SENT_FILE")" != "low" ]; then
            notify-send -u normal -i battery-low \
                "Low Battery Warning" \
                "Battery at ${CAPACITY}%. Please charge soon."
            echo "low" > "$NOTIFICATION_SENT_FILE"
            # Fix potential leader key issue after notification
            sleep 0.5
            /home/ng/.miozu/bin/fix-leader-key.sh > /dev/null 2>&1
        fi
    else
        # Reset notification state when battery is above low level
        rm -f "$NOTIFICATION_SENT_FILE"
    fi
else
    # Reset notification state when charging/charged
    rm -f "$NOTIFICATION_SENT_FILE"
fi