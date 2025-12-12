#!/bin/bash

echo "🎮 Black Desert Online Audio Fix"
echo "================================"

HEADPHONES_MAC="80:C3:BA:81:21:B4"
CARD_NAME="bluez_card.${HEADPHONES_MAC//:/_}"

/home/ng/.miozu/bin/fix-bluetooth-audio.sh

echo ""
echo "📌 Monitoring audio profile changes..."
echo "   Press Ctrl+C to stop when game is loaded"
echo ""

trap 'echo ""; echo "✓ Monitoring stopped"; exit 0' INT

while true; do
    CURRENT_PROFILE=$(pactl list cards 2>/dev/null | grep -A10 "$CARD_NAME" | grep "Active Profile" | cut -d: -f2 | xargs)
    
    if [[ "$CURRENT_PROFILE" != *"a2dp"* ]] && [[ -n "$CURRENT_PROFILE" ]]; then
        echo "⚠️  Profile switched to $CURRENT_PROFILE - fixing..."
        pactl set-card-profile "$CARD_NAME" a2dp-sink 2>/dev/null
        SINK_NAME="bluez_output.${HEADPHONES_MAC//:/_}.a2dp-sink"
        pactl set-default-sink "$SINK_NAME" 2>/dev/null
        echo "✓ Restored A2DP profile"
    fi
    
    sleep 2
done