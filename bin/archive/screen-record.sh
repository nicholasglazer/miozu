#!/bin/bash

# Screen recording wrapper for hsrx
# Usage: screen-record.sh [gif|video] [duration] [output_name] [--area] [--fps FPS]

HSRX_PATH="$HOME/code/foss/hsrx/hsrx"
VIDEOS_DIR="$HOME/Videos"
GIFS_DIR="$VIDEOS_DIR/gifs"
RECORDINGS_DIR="$VIDEOS_DIR/recordings"

# Default values
MODE="gif"
DURATION="10"
OUTPUT_NAME=""
AREA_SELECT=false
FPS="30"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --area)
            AREA_SELECT=true
            shift
            ;;
        --fps)
            FPS="$2"
            shift 2
            ;;
        *)
            if [[ -z "$PARSED_MODE" ]]; then
                MODE="$1"
                PARSED_MODE=true
            elif [[ -z "$PARSED_DURATION" ]]; then
                DURATION="$1"
                PARSED_DURATION=true
            elif [[ -z "$PARSED_NAME" ]]; then
                OUTPUT_NAME="$1"
                PARSED_NAME=true
            fi
            shift
            ;;
    esac
done


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

# Check if hsrx exists
if [ ! -f "$HSRX_PATH" ]; then
    echo "Error: hsrx not found at $HSRX_PATH"
    echo "Please compile hsrx first: cd ~/code/foss/hsrx && ghc -o hsrx Main.hs"
    exit 1
fi

# Create a temporary modified version of hsrx source and recompile
HSRX_DIR="$HOME/code/foss/hsrx"
BACKUP_FILE="$HSRX_DIR/Main.hs.backup"

# Backup original file
cp "$HSRX_DIR/Main.hs" "$BACKUP_FILE"

# Modify the source with our parameters
sed -i "s|time.*=.*\"[^\"]*\"|time = \"$DURATION\"|; s|outputFilePath.*=.*\"[^\"]*\"|outputFilePath = \"$OUTPUT_PATH\"|; s|fps.*=.*\"[^\"]*\"|fps = \"$FPS\"|" "$HSRX_DIR/Main.hs"

# Recompile hsrx with the modifications
cd "$HSRX_DIR"
if ! ghc -o hsrx Main.hs 2>/dev/null; then
    # If compilation fails, restore backup and exit
    cp "$BACKUP_FILE" "$HSRX_DIR/Main.hs"
    rm -f "$BACKUP_FILE"
    echo "Error: Failed to compile hsrx with modifications"
    exit 1
fi
cd - > /dev/null

# Show notification
notify-send "Screen Recording" "Starting ${MODE} recording for ${DURATION}s..." -i camera-video

# Run the recording
"$HSRX_PATH"

# Wait for recording to complete
sleep "$DURATION"

# Convert to GIF if needed
if [ "$MODE" = "gif" ] && [ -f "$OUTPUT_PATH" ]; then
    TEMP_MP4="$OUTPUT_PATH"
    GIF_PATH="${OUTPUT_PATH%.*}.gif"
    
    # Convert MP4 to GIF with optimization (use FPS from parameter)
    ffmpeg -i "$TEMP_MP4" -vf "fps=$FPS,scale=1024:-1:flags=lanczos,palettegen" -y "${TEMP_MP4}_palette.png" 2>/dev/null
    ffmpeg -i "$TEMP_MP4" -i "${TEMP_MP4}_palette.png" -filter_complex "fps=$FPS,scale=1024:-1:flags=lanczos[x];[x][1:v]paletteuse" -y "$GIF_PATH" 2>/dev/null
    
    # Clean up temporary files
    rm -f "$TEMP_MP4" "${TEMP_MP4}_palette.png"
    OUTPUT_PATH="$GIF_PATH"
fi

# Restore original hsrx source
cp "$BACKUP_FILE" "$HSRX_DIR/Main.hs"
rm -f "$BACKUP_FILE"

# Show completion notification
if [ -f "$OUTPUT_PATH" ]; then
    notify-send "Screen Recording" "Recording saved: $OUTPUT_PATH" -i camera-video
    echo "Recording saved: $OUTPUT_PATH"
else
    notify-send "Screen Recording" "Recording failed!" -i dialog-error
    echo "Recording failed!"
    exit 1
fi