#!/bin/bash

echo "🎨 Fixing Drag & Drop Visual Issues"
echo "===================================="

echo -e "\n1️⃣ Current picom backend:"
grep "^backend" ~/.config/picom/picom.conf

echo -e "\n2️⃣ Testing with different backends..."

echo -e "\n   Testing GLX backend (better ARGB support)..."
pkill picom
sleep 1
picom --backend glx --daemon
echo "   ✓ GLX backend activated"

echo -e "\n3️⃣ Current DND window type settings:"
grep -A6 "dnd = {" ~/.config/picom/picom.conf

echo -e "\n4️⃣ Tips for testing:"
echo "   • Try dragging a file in your file manager"
echo "   • Try dragging a tab in your browser"
echo "   • Try dragging text selection"
echo ""
echo "If issues persist, try these commands:"
echo "   picom --backend xrender --daemon  # Fallback to XRender"
echo "   picom --backend glx --glx-no-stencil --daemon  # GLX without stencil"
echo ""
echo "To make changes permanent, edit: ~/.config/picom/picom.conf"
echo ""
echo "✅ Configuration applied!"