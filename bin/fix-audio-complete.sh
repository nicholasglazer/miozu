#!/bin/bash

echo "🔊 Complete Audio System Fix for Meteor Lake + MOMENTUM 4"
echo "========================================================="

echo -e "\n1️⃣ Reconnecting Bluetooth headphones..."
bluetoothctl connect 80:C3:BA:81:21:B4 2>/dev/null
sleep 3

echo -e "\n2️⃣ Restarting PipeWire services..."
systemctl --user restart pipewire pipewire-pulse wireplumber
sleep 2

echo -e "\n3️⃣ Setting optimal audio configuration..."

# Create WirePlumber config to prevent Bluetooth codec switching
mkdir -p ~/.config/wireplumber/bluetooth.lua.d/
cat > ~/.config/wireplumber/bluetooth.lua.d/51-disable-hsp.lua << 'EOF'
rule = {
  matches = {
    {
      { "device.name", "matches", "bluez_card.*" },
    },
  },
  apply_properties = {
    ["bluez5.auto-connect"] = "[ a2dp_sink ]",
    ["bluez5.hw-volume"] = "[ a2dp_sink ]",
  },
}

table.insert(bluez_monitor.rules, rule)
EOF

# Create PipeWire config to increase buffer for SOF
mkdir -p ~/.config/pipewire/pipewire.conf.d/
cat > ~/.config/pipewire/pipewire.conf.d/10-sof-buffer.conf << 'EOF'
context.properties = {
    default.clock.rate          = 48000
    default.clock.allowed-rates = [ 48000 44100 ]
    default.clock.quantum       = 1024
    default.clock.min-quantum   = 512
    default.clock.max-quantum   = 2048
}

context.modules = [
    {   name = libpipewire-module-rt
        args = {
            nice.level    = -15
            rt.prio       = 88
            rt.time.soft  = 2000000
            rt.time.hard  = 2000000
        }
    }
]
EOF

echo -e "\n4️⃣ Applying ALSA fixes for SOF..."
sudo tee /etc/modprobe.d/sof-audio.conf > /dev/null << 'EOF'
# Intel SOF audio configuration for Meteor Lake
options snd_intel_dspcfg dsp_driver=3
options snd_sof sof_debug=0
options snd_sof_intel_hda_common hda_model=generic
EOF

echo -e "\n5️⃣ Checking Bluetooth connection..."
if bluetoothctl info 80:C3:BA:81:21:B4 | grep -q "Connected: yes"; then
    echo "✓ MOMENTUM 4 connected"
    
    # Force A2DP profile
    CARD_NAME="bluez_card.80_C3_BA_81_21_B4"
    if pactl list cards | grep -q "$CARD_NAME"; then
        pactl set-card-profile "$CARD_NAME" a2dp-sink 2>/dev/null || \
        pactl set-card-profile "$CARD_NAME" a2dp-sink-aptx 2>/dev/null || \
        pactl set-card-profile "$CARD_NAME" a2dp-sink-aac 2>/dev/null
        echo "✓ A2DP profile set"
    fi
else
    echo "⚠️  MOMENTUM 4 not connected"
fi

echo -e "\n6️⃣ Current audio status:"
echo "---"
wpctl status | grep -A5 "Sinks:"
echo "---"

echo -e "\n✅ Audio fix complete!"
echo ""
echo "If audio still doesn't work:"
echo "1. Turn your headphones off and on"
echo "2. Run: systemctl --user restart pipewire pipewire-pulse wireplumber"
echo "3. Check kernel version (need 6.10+): uname -r"
echo ""
echo "For BDO: Run ~/bin/bdo-audio-fix.sh before starting the game"