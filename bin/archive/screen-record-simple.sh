#!/bin/bash

# Simple screen recording with different selection methods
# Usage: screen-record-simple.sh [gif|video] [duration] [mode]
# mode: fullscreen, window, or area

VIDEOS_DIR="$HOME/Videos"
GIFS_DIR="$VIDEOS_DIR/gifs"
RECORDINGS_DIR="$VIDEOS_DIR/recordings"

# Default values
MODE="gif"
DURATION="10"
SELECTION_MODE="fullscreen"

# Parse arguments
if [ $# -ge 1 ]; then
    MODE="$1"
fi

if [ $# -ge 2 ]; then
    DURATION="$2"
fi

if [ $# -ge 3 ]; then
    SELECTION_MODE="$3"
fi

# Validate mode
if [ "$MODE" != "gif" ] && [ "$MODE" != "video" ]; then
    echo "Error: Mode must be 'gif' or 'video'"
    exit 1
fi

# Create directories
mkdir -p "$GIFS_DIR" "$RECORDINGS_DIR"

# Generate output filename
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
if [ "$MODE" = "gif" ]; then
    OUTPUT_PATH="$GIFS_DIR/recording_${TIMESTAMP}.gif"
else
    OUTPUT_PATH="$RECORDINGS_DIR/recording_${TIMESTAMP}.mp4"
fi

# Get recording parameters based on selection mode
if [ "$SELECTION_MODE" = "window" ]; then
    # Click on a window to record it
    notify-send "Screen Recording" "Click on window to record..." -i camera-video
    WINDOW_ID=$(xwininfo | grep "Window id:" | cut -d' ' -f4)
    GEOMETRY=$(xwininfo -id "$WINDOW_ID" | grep -E "geometry|Absolute upper-left")
    WIDTH=$(echo "$GEOMETRY" | grep geometry | sed 's/.*geometry \([0-9]*\)x.*/\1/')
    HEIGHT=$(echo "$GEOMETRY" | grep geometry | sed 's/.*geometry [0-9]*x\([0-9]*\).*/\1/')
    X=$(echo "$GEOMETRY" | grep "Absolute upper-left X" | sed 's/.*: //')
    Y=$(echo "$GEOMETRY" | grep "Absolute upper-left Y" | sed 's/.*: //')
    
    FFMPEG_INPUT="-f x11grab -video_size ${WIDTH}x${HEIGHT} -i :0.0+${X},${Y}"
    
elif [ "$SELECTION_MODE" = "area" ]; then
    # Try using hacksaw (alternative to slop) or fallback to fullscreen
    if command -v hacksaw >/dev/null 2>&1; then
        notify-send "Screen Recording" "Select area to record..." -i camera-video
        SELECTION=$(hacksaw -f "%x,%y %wx%h")
        if [ $? -eq 0 ] && [ -n "$SELECTION" ]; then
            GEOMETRY=${SELECTION% *}
            SIZE=${SELECTION#* }
            X=$(echo "$GEOMETRY" | cut -d',' -f1)
            Y=$(echo "$GEOMETRY" | cut -d',' -f2)
            WIDTH=$(echo "$SIZE" | cut -d'x' -f1)
            HEIGHT=$(echo "$SIZE" | cut -d'x' -f2)
            FFMPEG_INPUT="-f x11grab -video_size ${WIDTH}x${HEIGHT} -i :0.0+${X},${Y}"
        else
            echo "Area selection failed, using fullscreen"
            SELECTION_MODE="fullscreen"
        fi
    else
        echo "hacksaw not available, using fullscreen"
        SELECTION_MODE="fullscreen"
    fi
fi

# Fallback to fullscreen if area selection failed
if [ "$SELECTION_MODE" = "fullscreen" ]; then
    # Get screen resolution
    RESOLUTION=$(xrandr | grep '\*' | head -1 | awk '{print $1}')
    FFMPEG_INPUT="-f x11grab -video_size $RESOLUTION -i :0.0"
fi

# Show recording notification
notify-send "Screen Recording" "Recording ${MODE} for ${DURATION}s..." -i camera-video

# Record based on mode
if [ "$MODE" = "gif" ]; then
    # Record to temporary MP4 first
    TEMP_MP4="${OUTPUT_PATH%.*}_temp.mp4"
    
    ffmpeg $FFMPEG_INPUT -t "$DURATION" -y "$TEMP_MP4" 2>/dev/null &
    FFMPEG_PID=$!
    
    # Wait for recording to complete
    wait $FFMPEG_PID
    
    if [ -f "$TEMP_MP4" ]; then
        # Convert to optimized GIF (faster, smaller)
        ffmpeg -i "$TEMP_MP4" -vf "fps=10,scale=640:-1:flags=lanczos,palettegen=reserve_transparent=0" -y "${TEMP_MP4}_palette.png" 2>/dev/null
        ffmpeg -i "$TEMP_MP4" -i "${TEMP_MP4}_palette.png" -filter_complex "fps=10,scale=640:-1:flags=lanczos[x];[x][1:v]paletteuse=dither=bayer:bayer_scale=3" -y "$OUTPUT_PATH" 2>/dev/null
        
        # Clean up temporary files
        rm -f "$TEMP_MP4" "${TEMP_MP4}_palette.png"
    fi
else
    # Record video directly
    ffmpeg $FFMPEG_INPUT -t "$DURATION" -c:v libx264 -preset fast -crf 22 -y "$OUTPUT_PATH" 2>/dev/null &
    wait $!
fi

# Check result and copy to clipboard if it's a GIF
if [ -f "$OUTPUT_PATH" ]; then
    if [ "$MODE" = "gif" ]; then
        # Copy GIF to clipboard (convert first frame to PNG for compatibility)
        TEMP_PNG="/tmp/recording_clipboard.png"
        ffmpeg -i "$OUTPUT_PATH" -vf "select=eq(n\,0)" -q:v 1 -frames:v 1 -y "$TEMP_PNG" 2>/dev/null
        if [ -f "$TEMP_PNG" ]; then
            xclip -selection clipboard -t image/png -i "$TEMP_PNG" 2>/dev/null
            rm -f "$TEMP_PNG"
            notify-send "Screen Recording" "GIF saved and copied to clipboard: $(basename "$OUTPUT_PATH")" -i camera-video
            echo "GIF saved and copied to clipboard: $OUTPUT_PATH"
        else
            notify-send "Screen Recording" "GIF saved: $(basename "$OUTPUT_PATH")" -i camera-video
            echo "GIF saved: $OUTPUT_PATH"
        fi
    else
        notify-send "Screen Recording" "Video saved: $(basename "$OUTPUT_PATH")" -i camera-video
        echo "Video saved: $OUTPUT_PATH"
    fi
else
    notify-send "Screen Recording" "Recording failed!" -i dialog-error
    echo "Recording failed!"
    exit 1
fi