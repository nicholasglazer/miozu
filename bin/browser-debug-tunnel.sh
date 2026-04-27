#!/bin/bash
# Browser Debug Tunnel - Run on LOCAL machine to enable server-side Claude Code to control your browser
# Usage: browser-debug-tunnel.sh [user@server]

set -e

DEBUG_PORT=9222
CHROME_PROFILE="$HOME/.chrome-debug"
SERVER="${1:-ng@167.235.2.216}"  # Default server
SSH_PORT="${2:-2248}"              # Default SSH port

# Find Chrome/Chromium
CHROME=$(command -v chromium || command -v google-chrome || command -v chrome || echo "")
if [[ -z "$CHROME" ]]; then
    echo "ERROR: Chrome/Chromium not found!"
    echo "Install with: paru -S chromium"
    exit 1
fi

echo "=== Browser Debug Tunnel ==="
echo "Chrome: $CHROME"
echo "Debug Port: $DEBUG_PORT"
echo "Server: $SERVER (port $SSH_PORT)"
echo ""

# Check if Chrome debug server is already running
if curl -s "http://localhost:$DEBUG_PORT/json/version" &>/dev/null; then
    echo "[OK] Chrome debug server already running on port $DEBUG_PORT"
else
    echo "[..] Starting Chrome with remote debugging..."
    mkdir -p "$CHROME_PROFILE"

    "$CHROME" \
        --remote-debugging-port=$DEBUG_PORT \
        --user-data-dir="$CHROME_PROFILE" \
        --no-first-run \
        --remote-allow-origins=* \
        &>/dev/null &

    # Wait for Chrome to start
    for i in {1..10}; do
        if curl -s "http://localhost:$DEBUG_PORT/json/version" &>/dev/null; then
            echo "[OK] Chrome debug server ready"
            break
        fi
        sleep 0.5
    done

    if ! curl -s "http://localhost:$DEBUG_PORT/json/version" &>/dev/null; then
        echo "[FAIL] Chrome failed to start debug server"
        exit 1
    fi
fi

# Show Chrome version
echo ""
curl -s "http://localhost:$DEBUG_PORT/json/version" | grep -E '"Browser"|"V8"' | head -2
echo ""

# Start SSH tunnel (both directions)
echo "[..] Creating SSH tunnels to $SERVER..."
echo "     Remote: server:$DEBUG_PORT -> local:$DEBUG_PORT (browser debug)"
echo "     Local:  ports 5170-5179 -> server (dev servers)"
echo ""
echo "=== Tunnel Active ==="
echo "Claude Code on server can now use devtools_* MCP tools"
echo "Open http://localhost:5174 to access server's admin app"
echo "Press Ctrl+C to stop"
echo ""

# -R: reverse tunnel (server connects to our local port) - for browser debug
# -L: local tunnel (we connect to server's port) - for dev servers
# -N: no remote command, just tunnel
# -o ServerAliveInterval: keep connection alive
ssh -R $DEBUG_PORT:localhost:$DEBUG_PORT \
    -L 5170:localhost:5170 \
    -L 5171:localhost:5171 \
    -L 5172:localhost:5172 \
    -L 5173:localhost:5173 \
    -L 5174:localhost:5174 \
    -L 5175:localhost:5175 \
    -L 5176:localhost:5176 \
    -L 5177:localhost:5177 \
    -L 5178:localhost:5178 \
    -L 5179:localhost:5179 \
    -p $SSH_PORT \
    -N \
    -o ServerAliveInterval=60 \
    -o ServerAliveCountMax=3 \
    "$SERVER"
