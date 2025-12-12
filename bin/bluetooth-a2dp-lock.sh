#!/bin/bash

HEADPHONES_MAC="80:C3:BA:81:21:B4"

sudo tee /etc/pulse/default.pa.d/bluetooth-a2dp-lock.pa > /dev/null << 'EOF'
# Prevent automatic profile switching for MOMENTUM 4
# This forces A2DP and disables HSP/HFP auto-switching

# Disable role switching
load-module module-bluetooth-policy a2dp_source=false hfp_source=false auto_switch=0

# Set default to A2DP
set-card-profile bluez_card.80_C3_BA_81_21_B4 a2dp-sink
EOF

echo "✓ Created PulseAudio configuration to lock A2DP"

sudo tee /etc/udev/rules.d/99-bluetooth-audio.rules > /dev/null << 'EOF'
# Force A2DP for MOMENTUM 4 headphones
ACTION=="add", SUBSYSTEM=="bluetooth", ATTR{address}=="80:C3:BA:81:21:B4", RUN+="/home/ng/.miozu/bin/fix-bluetooth-audio.sh"
EOF

echo "✓ Created udev rule for automatic A2DP on connection"

echo ""
echo "Configuration applied. You may need to:"
echo "1. Restart PulseAudio: systemctl --user restart pulseaudio"
echo "2. Reconnect your headphones"