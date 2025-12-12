#!/bin/bash
# Steam wrapper that prevents Super key grabbing
export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0
export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOST=0

# Kill any existing Steam input processes
pkill -f "steamwebhelper" 2>/dev/null || true

# Launch Steam with environment variables
exec /usr/bin/steam "$@"
