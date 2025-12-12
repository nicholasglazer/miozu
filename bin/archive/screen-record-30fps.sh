#!/bin/bash

# Simple 30fps screen recorder using ffmpeg directly
# Usage: screen-record-30fps.sh [duration] [output_name] [fps]

DURATION="${1:-10}"
OUTPUT_NAME="${2:-recording_$(date +%Y%m%d_%H%M%S)}"
FPS="${3:-30}"

VIDEOS_DIR="$HOME/Videos"
GIFS_DIR="$VIDEOS_DIR/gifs"
RECORDINGS_DIR="$VIDEOS_DIR/recordings"

# Create directories
mkdir -p "$GIFS_DIR" "$RECORDINGS_DIR"

# Get mouse coordinates to determine active monitor
eval $(xdotool getmouselocation --shell)
MOUSE_X=$X
MOUSE_Y=$Y

# Get monitor info from xrandr
MONITOR_INFO=$(xrandr | grep " connected " | while read -r line; do
    if [[ $line =~ ([0-9]+)x([0-9]+)\+([0-9]+)\+([0-9]+) ]]; then
        WIDTH=${BASH_REMATCH[1]}
        HEIGHT=${BASH_REMATCH[2]}
        OFFSET_X=${BASH_REMATCH[3]}
        OFFSET_Y=${BASH_REMATCH[4]}
        MAX_X=$((OFFSET_X + WIDTH))
        MAX_Y=$((OFFSET_Y + HEIGHT))

        if (( MOUSE_X >= OFFSET_X && MOUSE_X <= MAX_X && MOUSE_Y >= OFFSET_Y && MOUSE_Y <= MAX_Y )); then
            echo "$WIDTH $HEIGHT $OFFSET_X $OFFSET_Y"
            break
        fi
    fi
done)

if [ -z "$MONITOR_INFO" ]; then
    echo "Could not detect active monitor, using full screen"
    MONITOR_INFO="1920 1080 0 0"
fi

read WIDTH HEIGHT OFFSET_X OFFSET_Y <<< "$MONITOR_INFO"

# Determine output format and path
if [[ "$OUTPUT_NAME" == *.gif ]]; then
    OUTPUT_PATH="$GIFS_DIR/$OUTPUT_NAME"
    MODE="gif"
else
    OUTPUT_PATH="$RECORDINGS_DIR/$OUTPUT_NAME.mp4"
    MODE="video"
fi

echo "Recording ${WIDTH}x${HEIGHT} at $FPS fps for ${DURATION}s to $OUTPUT_PATH"
notify-send "Screen Recording" "Starting ${MODE} recording at ${FPS}fps for ${DURATION}s..." -i camera-video

# Record with ffmpeg
ffmpeg -f x11grab -video_size "${WIDTH}x${HEIGHT}" -framerate "$FPS" -i ":0.0+${OFFSET_X},${OFFSET_Y}" -t "$DURATION" -y "$OUTPUT_PATH" 2>/dev/null

# Convert to GIF if needed
if [ "$MODE" = "gif" ] && [ -f "$OUTPUT_PATH" ]; then
    TEMP_MP4="${OUTPUT_PATH%.*}.mp4"
    mv "$OUTPUT_PATH" "$TEMP_MP4"

    # Create optimized GIF
    ffmpeg -i "$TEMP_MP4" -vf "fps=$FPS,scale=1024:-1:flags=lanczos,palettegen" -y "${TEMP_MP4}_palette.png" 2>/dev/null
    ffmpeg -i "$TEMP_MP4" -i "${TEMP_MP4}_palette.png" -filter_complex "fps=$FPS,scale=1024:-1:flags=lanczos[x];[x][1:v]paletteuse" -y "$OUTPUT_PATH" 2>/dev/null

    # Clean up
    rm -f "$TEMP_MP4" "${TEMP_MP4}_palette.png"
fi

# Show completion notification
if [ -f "$OUTPUT_PATH" ]; then
    notify-send "Screen Recording" "Recording saved: $OUTPUT_PATH" -i camera-video
    echo "Recording saved: $OUTPUT_PATH"
else
    notify-send "Screen Recording" "Recording failed!" -i dialog-error
    echo "Recording failed!"
    exit 1
fi