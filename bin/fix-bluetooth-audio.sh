#!/bin/bash

HEADPHONES_MAC="80:C3:BA:81:21:B4"
CARD_NAME="bluez_card.${HEADPHONES_MAC//:/_}"

echo "🎧 Fixing MOMENTUM 4 Bluetooth audio codec..."

if ! pactl list cards short | grep -q "$CARD_NAME"; then
    echo "❌ Headphones not connected. Attempting to reconnect..."
    
    bluetoothctl disconnect "$HEADPHONES_MAC" 2>/dev/null
    sleep 2
    bluetoothctl connect "$HEADPHONES_MAC"
    sleep 3
    
    if ! pactl list cards short | grep -q "$CARD_NAME"; then
        echo "❌ Failed to connect headphones"
        exit 1
    fi
fi

echo "✓ Headphones connected"

AVAILABLE_PROFILES=$(pactl list cards | grep -A20 "$CARD_NAME" | grep -E "a2dp-sink|headset-head-unit" | grep "available: yes")

if echo "$AVAILABLE_PROFILES" | grep -q "a2dp-sink"; then
    echo "→ Switching to A2DP (High Quality Audio)..."
    
    # Try aptX HD first (highest quality)
    if pactl list cards | grep -A30 "$CARD_NAME" | grep -q "a2dp-sink:.*aptX HD.*available: yes"; then
        pactl set-card-profile "$CARD_NAME" a2dp-sink
        echo "  Using aptX HD codec"
    # Try regular aptX
    elif pactl list cards | grep -A30 "$CARD_NAME" | grep -q "a2dp-sink-aptx:.*available: yes"; then
        pactl set-card-profile "$CARD_NAME" a2dp-sink-aptx
        echo "  Using aptX codec"
    # Try AAC
    elif pactl list cards | grep -A30 "$CARD_NAME" | grep -q "a2dp-sink-aac:.*available: yes"; then
        pactl set-card-profile "$CARD_NAME" a2dp-sink-aac
        echo "  Using AAC codec"
    # Fall back to SBC
    else
        pactl set-card-profile "$CARD_NAME" a2dp-sink-sbc
        echo "  Using SBC codec"
    fi
    
    sleep 1
    
    SINK_NAME="bluez_output.${HEADPHONES_MAC//:/_}.a2dp-sink"
    pactl set-default-sink "$SINK_NAME"
    
    echo "✓ Switched to A2DP high quality audio"
else
    echo "⚠️  A2DP not available, forcing reconnection..."
    
    bluetoothctl disconnect "$HEADPHONES_MAC"
    sleep 3
    bluetoothctl connect "$HEADPHONES_MAC"
    sleep 4
    
    if pactl list cards | grep -A20 "$CARD_NAME" | grep -q "a2dp-sink.*available: yes"; then
        pactl set-card-profile "$CARD_NAME" a2dp-sink
        SINK_NAME="bluez_output.${HEADPHONES_MAC//:/_}.a2dp-sink"
        pactl set-default-sink "$SINK_NAME"
        echo "✓ Successfully switched to A2DP after reconnection"
    else
        echo "❌ A2DP profile still not available. Your headphones might be in call mode."
        echo "   Try turning them off and on again, or check if another app is using them."
    fi
fi

CURRENT_PROFILE=$(pactl list cards | grep -A10 "$CARD_NAME" | grep "Active Profile" | cut -d: -f2 | xargs)
echo ""
echo "Current profile: $CURRENT_PROFILE"

if [[ "$CURRENT_PROFILE" == *"a2dp"* ]]; then
    echo "✅ Audio codec fixed successfully!"
    notify-send "🎧 MOMENTUM 4" "Switched to high quality audio (A2DP)" -t 3000
else
    echo "⚠️  Still using HSP/HFP profile. Manual intervention may be needed."
    notify-send "🎧 MOMENTUM 4" "Warning: Still in HSP/HFP mode" -t 5000
fi