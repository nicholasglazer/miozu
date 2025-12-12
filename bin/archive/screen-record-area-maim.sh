#!/bin/bash

# Screen recording with area selection using maim (like screenshot workflow)
# Usage: screen-record-area-maim.sh [duration] [output_name]

VIDEOS_DIR="$HOME/Videos"
GIFS_DIR="$VIDEOS_DIR/gifs"

# Default values
DURATION="5"
OUTPUT_NAME=""

# Parse arguments
if [ $# -ge 1 ]; then
    DURATION="$1"
fi

if [ $# -ge 2 ]; then
    OUTPUT_NAME="$2"
fi

# Create directories
mkdir -p "$GIFS_DIR"

# Generate output filename
if [ -z "$OUTPUT_NAME" ]; then
    TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
    OUTPUT_NAME="selection_${TIMESTAMP}.gif"
fi
OUTPUT_PATH="$GIFS_DIR/$OUTPUT_NAME"

# Get area selection using maim (same as your screenshot workflow)
notify-send "Screen Recording" "Select area to record..." -i camera-video

# Use maim to get selection geometry (same method as your screenshots)
SELECTION=$(maim -s -f png /dev/null 2>&1 | grep -o 'Geometry: [0-9]*x[0-9]*+[0-9]*+[0-9]*' | cut -d' ' -f2)

if [ $? -ne 0 ] || [ -z "$SELECTION" ]; then
    notify-send "Screen Recording" "Selection cancelled" -i dialog-error
    echo "Selection cancelled"
    exit 1
fi

# Parse geometry: WIDTHxHEIGHT+X+Y
WIDTH=$(echo "$SELECTION" | cut -d'x' -f1)
HEIGHT=$(echo "$SELECTION" | cut -d'x' -f2 | cut -d'+' -f1)
X=$(echo "$SELECTION" | cut -d'+' -f2)
Y=$(echo "$SELECTION" | cut -d'+' -f3)

echo "Recording area: ${WIDTH}x${HEIGHT} at ${X},${Y} for ${DURATION}s"

# Show recording notification
notify-send "Screen Recording" "Recording selected area for ${DURATION}s..." -i camera-video

# Record to temporary MP4 first (for better quality control)
TEMP_MP4="${OUTPUT_PATH%.*}_temp.mp4"

# Record with high quality settings
ffmpeg -f x11grab -video_size "${WIDTH}x${HEIGHT}" -framerate 30 -i ":0.0+${X},${Y}" \
    -t "$DURATION" -c:v libx264 -preset fast -crf 18 -pix_fmt yuv420p \
    -y "$TEMP_MP4" 2>/dev/null &

FFMPEG_PID=$!
wait $FFMPEG_PID

if [ -f "$TEMP_MP4" ]; then
    # Convert to high-quality GIF optimized for web
    # Two-pass encoding for better quality/size balance
    
    # Pass 1: Generate optimized palette
    ffmpeg -i "$TEMP_MP4" \
        -vf "fps=20,scale=1024:-1:flags=lanczos,palettegen=reserve_transparent=0:max_colors=128" \
        -y "${TEMP_MP4}_palette.png" 2>/dev/null
    
    # Pass 2: Create GIF with optimized settings for web
    ffmpeg -i "$TEMP_MP4" -i "${TEMP_MP4}_palette.png" \
        -filter_complex "fps=20,scale=1024:-1:flags=lanczos[x];[x][1:v]paletteuse=dither=bayer:bayer_scale=2:diff_mode=rectangle" \
        -y "$OUTPUT_PATH" 2>/dev/null
    
    # Clean up temporary files
    rm -f "$TEMP_MP4" "${TEMP_MP4}_palette.png"
    
    if [ -f "$OUTPUT_PATH" ]; then
        # Get file size for reporting
        FILE_SIZE=$(du -h "$OUTPUT_PATH" | cut -f1)
        
        # Copy to clipboard (first frame as PNG for compatibility)
        TEMP_PNG="/tmp/recording_clipboard.png"
        ffmpeg -i "$OUTPUT_PATH" -vf "select=eq(n\,0)" -q:v 1 -frames:v 1 -y "$TEMP_PNG" 2>/dev/null
        if [ -f "$TEMP_PNG" ]; then
            xclip -selection clipboard -t image/png -i "$TEMP_PNG" 2>/dev/null
            rm -f "$TEMP_PNG"
        fi
        
        notify-send "Screen Recording" "GIF saved (${FILE_SIZE}) and copied to clipboard!" -i camera-video
        echo "GIF saved: $OUTPUT_PATH (${FILE_SIZE}) and copied to clipboard"
    else
        notify-send "Screen Recording" "GIF conversion failed!" -i dialog-error
        echo "GIF conversion failed!"
        exit 1
    fi
else
    notify-send "Screen Recording" "Recording failed!" -i dialog-error
    echo "Recording failed!"
    exit 1
fi