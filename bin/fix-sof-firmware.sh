#!/bin/bash

echo "🔧 Fixing SOF Firmware Topology for Meteor Lake"
echo "==============================================="

echo -e "\n1️⃣ Creating symlinks for SOF ACE topology (Meteor Lake uses ACE)..."
sudo ln -sf /lib/firmware/intel/sof-ipc4-tplg /lib/firmware/intel/sof-ace-tplg 2>/dev/null || true

echo -e "\n2️⃣ Verifying topology file exists..."
if [ -f "/lib/firmware/intel/sof-ace-tplg/sof-hda-generic-2ch.tplg" ]; then
    echo "✓ Topology file found"
else
    echo "❌ Topology file missing, creating fallback..."
    sudo cp /lib/firmware/intel/sof-ipc4-tplg/sof-hda-generic-2ch.tplg \
            /lib/firmware/intel/sof-ace-tplg/sof-hda-generic-2ch.tplg 2>/dev/null || true
fi

echo -e "\n3️⃣ Fixing kernel module options..."
sudo tee /etc/modprobe.d/sof-meteor-lake.conf > /dev/null << 'EOF'
# Intel SOF configuration for Meteor Lake
options snd_intel_dspcfg dsp_driver=3
options snd_sof sof_debug=0
options snd_sof_intel_hda_common hda_model=generic
options snd_hda_intel probe_mask=1
# Increase buffer for Meteor Lake
options snd_sof_pci fw_path="intel/sof-ipc4/mtl"
options snd_sof_pci tplg_path="intel/sof-ace-tplg"
EOF

echo -e "\n4️⃣ Reloading audio modules..."
sudo modprobe -r snd_sof_pci_intel_mtl 2>/dev/null || true
sudo modprobe -r snd_sof_intel_hda_common 2>/dev/null || true
sudo modprobe -r snd_sof 2>/dev/null || true
sleep 1
sudo modprobe snd_sof
sudo modprobe snd_sof_intel_hda_common
sudo modprobe snd_sof_pci_intel_mtl

echo -e "\n5️⃣ Checking dmesg for SOF errors..."
echo "Recent SOF messages:"
sudo dmesg | grep -i sof | tail -5

echo -e "\n✅ SOF firmware fix applied!"
echo ""
echo "Please restart PipeWire:"
echo "systemctl --user restart pipewire pipewire-pulse wireplumber"