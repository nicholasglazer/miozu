#!/bin/bash

# Screen recording with area selection (like screenshot workflow)
# Usage: screen-record-area.sh [gif|video] [duration] [output_name]

VIDEOS_DIR="$HOME/Videos"
GIFS_DIR="$VIDEOS_DIR/gifs"
RECORDINGS_DIR="$VIDEOS_DIR/recordings"

# Default values
MODE="gif"
DURATION="10"
OUTPUT_NAME=""
CLIPBOARD=false

# Parse arguments
if [ $# -ge 1 ]; then
    MODE="$1"
fi

if [ $# -ge 2 ]; then
    DURATION="$2"
fi

if [ $# -ge 3 ]; then
    OUTPUT_NAME="$3"
fi

# Check for clipboard flag
if [[ "$*" == *"--clipboard"* ]]; then
    CLIPBOARD=true
fi

# Validate mode
if [ "$MODE" != "gif" ] && [ "$MODE" != "video" ]; then
    echo "Error: Mode must be 'gif' or 'video'"
    exit 1
fi

# Create directories if they don't exist
mkdir -p "$GIFS_DIR" "$RECORDINGS_DIR"

# Generate output filename if not provided
if [ -z "$OUTPUT_NAME" ]; then
    TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
    if [ "$MODE" = "gif" ]; then
        OUTPUT_NAME="recording_${TIMESTAMP}.gif"
        OUTPUT_PATH="$GIFS_DIR/$OUTPUT_NAME"
    else
        OUTPUT_NAME="recording_${TIMESTAMP}.mp4"
        OUTPUT_PATH="$RECORDINGS_DIR/$OUTPUT_NAME"
    fi
else
    if [ "$MODE" = "gif" ]; then
        OUTPUT_PATH="$GIFS_DIR/$OUTPUT_NAME"
    else
        OUTPUT_PATH="$RECORDINGS_DIR/$OUTPUT_NAME"
    fi
fi

# Get area selection using slop (similar to maim -s)
echo "Select recording area with mouse..."
SELECTION=$(slop -f "%x,%y %wx%h")

if [ $? -ne 0 ] || [ -z "$SELECTION" ]; then
    echo "Area selection cancelled"
    exit 1
fi

# Parse selection: "x,y widthxheight"
GEOMETRY=${SELECTION% *}  # "x,y"
SIZE=${SELECTION#* }      # "widthxheight"

# Split geometry into x,y
X=$(echo "$GEOMETRY" | cut -d',' -f1)
Y=$(echo "$GEOMETRY" | cut -d',' -f2)

# Split size into width x height  
WIDTH=$(echo "$SIZE" | cut -d'x' -f1)
HEIGHT=$(echo "$SIZE" | cut -d'x' -f2)

# Show notification
notify-send "Screen Recording" "Starting ${MODE} recording of selected area for ${DURATION}s..." -i camera-video

# Record using FFmpeg directly with area selection
if [ "$MODE" = "gif" ]; then
    # Record to temporary MP4 first
    TEMP_MP4="${OUTPUT_PATH%.*}_temp.mp4"
    
    ffmpeg -f x11grab -video_size "${WIDTH}x${HEIGHT}" -framerate 30 -i ":0.0+${X},${Y}" -t "$DURATION" -y "$TEMP_MP4" &
    FFMPEG_PID=$!
    
    # Wait for recording to complete
    wait $FFMPEG_PID
    
    if [ -f "$TEMP_MP4" ]; then
        # Convert to optimized GIF
        ffmpeg -i "$TEMP_MP4" -vf "fps=15,scale=1024:-1:flags=lanczos,palettegen" -y "${TEMP_MP4}_palette.png" 2>/dev/null
        ffmpeg -i "$TEMP_MP4" -i "${TEMP_MP4}_palette.png" -filter_complex "fps=15,scale=1024:-1:flags=lanczos[x];[x][1:v]paletteuse" -y "$OUTPUT_PATH" 2>/dev/null
        
        # Clean up temporary files
        rm -f "$TEMP_MP4" "${TEMP_MP4}_palette.png"
    fi
else
    # Record video directly
    ffmpeg -f x11grab -video_size "${WIDTH}x${HEIGHT}" -framerate 30 -i ":0.0+${X},${Y}" -t "$DURATION" -c:v libx264 -preset fast -crf 22 -y "$OUTPUT_PATH" &
    FFMPEG_PID=$!
    
    # Wait for recording to complete
    wait $FFMPEG_PID
fi

# Check if recording was successful
if [ -f "$OUTPUT_PATH" ]; then
    # Copy to clipboard if requested and it's a GIF
    if [ "$CLIPBOARD" = true ] && [ "$MODE" = "gif" ]; then
        # Convert GIF to PNG for clipboard (more compatible)
        TEMP_PNG="/tmp/recording_clipboard.png"
        ffmpeg -i "$OUTPUT_PATH" -vf "select=eq(n\,0)" -q:v 1 -frames:v 1 -y "$TEMP_PNG" 2>/dev/null
        xclip -selection clipboard -t image/png -i "$TEMP_PNG" 2>/dev/null
        rm -f "$TEMP_PNG"
        
        notify-send "Screen Recording" "GIF saved: $OUTPUT_PATH and copied to clipboard" -i camera-video
        echo "GIF saved: $OUTPUT_PATH and copied to clipboard"
    else
        notify-send "Screen Recording" "Recording saved: $OUTPUT_PATH" -i camera-video
        echo "Recording saved: $OUTPUT_PATH"
    fi
else
    notify-send "Screen Recording" "Recording failed!" -i dialog-error
    echo "Recording failed!"
    exit 1
fi