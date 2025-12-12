#!/bin/bash

echo "🔧 ArchWiki-Based Audio Fix for PipeWire + Bluetooth"
echo "====================================================="

echo -e "\n1️⃣ Disabling WirePlumber auto-switch to HSP/HFP (ArchWiki recommendation)..."
wpctl settings --save bluetooth.autoswitch-to-headset-profile false
echo "✓ Auto-switch disabled"

echo -e "\n2️⃣ Creating WirePlumber Bluetooth configuration..."
mkdir -p ~/.config/wireplumber/main.lua.d/
cat > ~/.config/wireplumber/main.lua.d/51-bluetooth-config.lua << 'EOF'
-- Disable HSP/HFP profiles and force A2DP
rule = {
  matches = {
    {
      { "device.name", "matches", "bluez_card.*" },
    },
  },
  apply_properties = {
    ["bluez5.auto-connect"] = "[ a2dp_sink ]",
    ["bluez5.hw-volume"] = "[ a2dp_sink ]",
    ["bluez5.a2dp.force-audio-info"] = true,
    ["bluez5.a2dp.forced-codec"] = "aptx_hd",
  },
}

table.insert(bluez_monitor.rules, rule)
EOF

echo -e "\n3️⃣ Creating PipeWire configuration for SOF issues..."
mkdir -p ~/.config/pipewire/pipewire.conf.d/
cat > ~/.config/pipewire/pipewire.conf.d/20-fix-sof.conf << 'EOF'
# Fix for SOF "Broken pipe" errors per ArchWiki
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

stream.properties = {
    resample.quality      = 10
    channelmix.normalize  = false
    channelmix.mix-lfe    = false
    session.suspend-timeout-seconds = 0
}
EOF

echo -e "\n4️⃣ Fixing Bluetooth codecs configuration..."
mkdir -p ~/.config/pipewire/pipewire-pulse.conf.d/
cat > ~/.config/pipewire/pipewire-pulse.conf.d/20-bluetooth.conf << 'EOF'
# Bluetooth configuration per ArchWiki
pulse.properties = {
    pulse.format = "F32"
}

stream.properties = {
    stream.dont-remix = true
    resample.disable = false
}
EOF

echo -e "\n5️⃣ Clearing Bluetooth cache (ArchWiki fix for missing A2DP)..."
echo "⚠️  This will unpair all Bluetooth devices!"
read -p "Continue? (y/n): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    sudo systemctl stop bluetooth
    sudo rm -rf /var/lib/bluetooth/*
    sudo systemctl start bluetooth
    echo "✓ Bluetooth cache cleared. You need to re-pair your devices."
else
    echo "⏭️  Skipping Bluetooth cache clear"
fi

echo -e "\n6️⃣ Restarting audio services..."
systemctl --user restart pipewire pipewire-pulse wireplumber

echo -e "\n7️⃣ Checking current status..."
sleep 2
echo "WirePlumber auto-switch setting:"
wpctl settings | grep "bluetooth.autoswitch"
echo ""
echo "PipeWire status:"
systemctl --user status pipewire --no-pager | head -5
echo ""
echo "Available sinks:"
pactl list short sinks

echo -e "\n✅ Fix applied based on ArchWiki recommendations!"
echo ""
echo "Next steps:"
echo "1. Re-pair your MOMENTUM 4 headphones"
echo "2. Use: pactl set-card-profile bluez_card.80_C3_BA_81_21_B4 a2dp-sink"
echo "3. If issues persist, check: journalctl --user -u pipewire -u wireplumber -f"