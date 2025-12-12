#!/bin/bash

# Toggle screen recording - start/stop with the same hotkey
# This script manages recording state using PID files

VIDEOS_DIR="$HOME/Videos"
RECORDINGS_DIR="$VIDEOS_DIR/recordings"
PID_FILE="/tmp/screen-recording.pid"
INFO_FILE="/tmp/screen-recording.info"

# Create directories if they don't exist
mkdir -p "$RECORDINGS_DIR"

# Check if recording is already running
if [ -f "$PID_FILE" ]; then
    # Recording is active - STOP it
    FFMPEG_PID=$(cat "$PID_FILE")

    if ps -p "$FFMPEG_PID" > /dev/null 2>&1; then
        # Kill the ffmpeg process gracefully
        kill -SIGINT "$FFMPEG_PID" 2>/dev/null
        sleep 0.5

        # Force kill if still running
        if ps -p "$FFMPEG_PID" > /dev/null 2>&1; then
            kill -9 "$FFMPEG_PID" 2>/dev/null
        fi

        # Get output path from info file
        if [ -f "$INFO_FILE" ]; then
            OUTPUT_PATH=$(cat "$INFO_FILE")
            rm -f "$INFO_FILE"

            # Show completion notification
            if [ -f "$OUTPUT_PATH" ]; then
                notify-send "Screen Recording" "Recording stopped and saved:\n$OUTPUT_PATH" -i camera-video
                echo "Recording saved: $OUTPUT_PATH"
            else
                notify-send "Screen Recording" "Recording stopped" -i camera-video
            fi
        else
            notify-send "Screen Recording" "Recording stopped" -i camera-video
        fi

        rm -f "$PID_FILE"
        exit 0
    else
        # PID file exists but process is dead - clean up
        rm -f "$PID_FILE" "$INFO_FILE"
    fi
fi

# No recording running - START new recording
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
OUTPUT_NAME="recording_${TIMESTAMP}.mp4"
OUTPUT_PATH="$RECORDINGS_DIR/$OUTPUT_NAME"

# Get active monitor info using xrandr and mouse position
get_active_monitor() {
    # Get mouse coordinates
    eval $(xdotool getmouselocation --shell)
    MOUSE_X=$X
    MOUSE_Y=$Y

    # Parse xrandr output to find which monitor contains the mouse
    xrandr --current | grep " connected" | while read -r line; do
        if [[ $line =~ ([0-9]+)x([0-9]+)\+([0-9]+)\+([0-9]+) ]]; then
            WIDTH="${BASH_REMATCH[1]}"
            HEIGHT="${BASH_REMATCH[2]}"
            OFFSET_X="${BASH_REMATCH[3]}"
            OFFSET_Y="${BASH_REMATCH[4]}"
            MAX_X=$((OFFSET_X + WIDTH))
            MAX_Y=$((OFFSET_Y + HEIGHT))

            if [ "$MOUSE_X" -ge "$OFFSET_X" ] && [ "$MOUSE_X" -le "$MAX_X" ] && \
               [ "$MOUSE_Y" -ge "$OFFSET_Y" ] && [ "$MOUSE_Y" -le "$MAX_Y" ]; then
                echo "$WIDTH $HEIGHT $OFFSET_X $OFFSET_Y"
                return
            fi
        fi
    done
}

# Get monitor geometry
MONITOR_INFO=$(get_active_monitor)
if [ -z "$MONITOR_INFO" ]; then
    notify-send "Screen Recording" "Failed to detect active monitor" -i dialog-error
    exit 1
fi

read WIDTH HEIGHT OFFSET_X OFFSET_Y <<< "$MONITOR_INFO"

# Start recording in background
notify-send "Screen Recording" "Recording started on ${WIDTH}x${HEIGHT} monitor\nPress hotkey again to stop" -i camera-video

# PRODUCT DEMO PRESET - Optimized for CEO/Developer use case
# Perfect for showing products on social networks with high quality
#
# Quality settings:
# - H.264 Main profile (better quality than baseline, still widely compatible)
# - YUV420p pixel format (universal compatibility)
# - CRF 20 (sweet spot: no artifacts, smooth, compact size)
# - Tune animation (optimized for UI/product demos vs real video)
# - Medium preset (good compression without slowdown)
# - Faststart (instant playback on Telegram/web)
# - 30fps (smooth for most UI animations)
#
# Expected results:
# - Size: ~35-45MB per minute (perfect for social networks)
# - Quality: No artifacts, no pixelation, sharp text
# - Compatibility: Telegram, LinkedIn, Twitter, WhatsApp, all phones
ffmpeg -f x11grab \
    -video_size "${WIDTH}x${HEIGHT}" \
    -framerate 30 \
    -i ":0.0+${OFFSET_X},${OFFSET_Y}" \
    -vf "fps=30,scale=1920:-1:flags=lanczos,format=yuv420p" \
    -c:v libx264 \
    -profile:v main \
    -level 4.0 \
    -preset medium \
    -tune animation \
    -crf 20 \
    -g 60 \
    -movflags +faststart \
    -pix_fmt yuv420p \
    -y "$OUTPUT_PATH" \
    > /dev/null 2>&1 &

FFMPEG_PID=$!

# Save PID and output path
echo "$FFMPEG_PID" > "$PID_FILE"
echo "$OUTPUT_PATH" > "$INFO_FILE"

echo "Recording started with PID: $FFMPEG_PID"
echo "Output will be saved to: $OUTPUT_PATH"
echo "Press the same hotkey to stop recording"
