#!/bin/bash

# Screen recording with multiple presets
# Usage: screen-record-presets.sh [preset]
# Presets: product (default), tutorial, gif, bug, area

VIDEOS_DIR="$HOME/Videos"
RECORDINGS_DIR="$VIDEOS_DIR/recordings"
GIFS_DIR="$VIDEOS_DIR/gifs"
PID_FILE="/tmp/screen-recording.pid"
INFO_FILE="/tmp/screen-recording.info"
PRESET_FILE="/tmp/screen-recording.preset"
LOCK_FILE="/tmp/screen-recording.lock"
LOG_FILE="$HOME/.miozu/logs/screen-recording.log"

# Get preset from argument (default: product)
PRESET="${1:-product}"

# Create directories
mkdir -p "$RECORDINGS_DIR" "$GIFS_DIR" "$HOME/.miozu/logs"

# Cleanup function for trap
cleanup() {
    flock -u 200 2>/dev/null
}
trap cleanup EXIT

# Acquire lock to prevent race conditions
exec 200>"$LOCK_FILE"
if ! flock -n 200; then
    notify-send "Screen Recording" "Another recording operation in progress" -i dialog-information
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Lock acquisition failed - another instance running" >> "$LOG_FILE"
    exit 1
fi

# Log script start
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Script started with preset: $PRESET" >> "$LOG_FILE"

# Validate required tools
for tool in xdotool xrandr ffmpeg notify-send; do
    if ! command -v "$tool" &> /dev/null; then
        notify-send "Screen Recording" "Error: Required tool '$tool' not found" -i dialog-error
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: Required tool '$tool' not found" >> "$LOG_FILE"
        exit 1
    fi
done

# Check if recording is already running
if [ -f "$PID_FILE" ]; then
    # Recording is active - STOP it
    FFMPEG_PID=$(cat "$PID_FILE")
    CURRENT_PRESET=$(cat "$PRESET_FILE" 2>/dev/null || echo "product")
    START_TIME=$(stat -c %Y "$PID_FILE" 2>/dev/null || echo "0")

    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Stop request received for PID: $FFMPEG_PID, Preset: $CURRENT_PRESET" >> "$LOG_FILE"

    if ps -p "$FFMPEG_PID" > /dev/null 2>&1; then
        # Kill the ffmpeg process gracefully
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Sending SIGINT to ffmpeg process..." >> "$LOG_FILE"
        kill -SIGINT "$FFMPEG_PID" 2>/dev/null

        # Wait up to 5 seconds for ffmpeg to finalize the file properly
        # FFmpeg needs time to write the moov atom (MP4 index) and close the file
        for i in {1..10}; do
            sleep 0.5
            if ! ps -p "$FFMPEG_PID" > /dev/null 2>&1; then
                echo "[$(date '+%Y-%m-%d %H:%M:%S')] FFmpeg exited gracefully after ${i} iterations" >> "$LOG_FILE"
                break
            fi
        done

        # Force kill if still running after 5 seconds
        if ps -p "$FFMPEG_PID" > /dev/null 2>&1; then
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] Process still running after 5s, sending SIGKILL..." >> "$LOG_FILE"
            kill -9 "$FFMPEG_PID" 2>/dev/null
        fi

        # Get output path from info file
        if [ -f "$INFO_FILE" ]; then
            OUTPUT_PATH=$(cat "$INFO_FILE")
            rm -f "$INFO_FILE"

            # Calculate duration
            END_TIME=$(date +%s)
            if [ "$START_TIME" -gt 0 ]; then
                DURATION=$((END_TIME - START_TIME))
                DURATION_FMT=$(printf "%02d:%02d" $((DURATION/60)) $((DURATION%60)))
            else
                DURATION_FMT="--:--"
            fi

            # Show completion notification
            if [ -f "$OUTPUT_PATH" ]; then
                FILE_SIZE=$(du -h "$OUTPUT_PATH" | cut -f1)
                FILE_NAME=$(basename "$OUTPUT_PATH")
                notify-send "⏹️ STOPPED ($DURATION_FMT)" "$FILE_NAME\nSize: $FILE_SIZE" -i camera-video -u normal
                echo "[$(date '+%Y-%m-%d %H:%M:%S')] Recording saved successfully: $OUTPUT_PATH ($FILE_SIZE, $DURATION_FMT)" >> "$LOG_FILE"
                echo "Recording saved: $OUTPUT_PATH"
            else
                notify-send "⚠️ RECORDING FAILED" "File not found after stop" -i dialog-warning -u critical
                echo "[$(date '+%Y-%m-%d %H:%M:%S')] WARNING: Recording stopped but file not found: $OUTPUT_PATH" >> "$LOG_FILE"
            fi
        else
            notify-send "⏹️ RECORDING STOPPED" "No output file info" -i camera-video -u normal
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] Recording stopped (no info file found)" >> "$LOG_FILE"
        fi

        rm -f "$PID_FILE" "$PRESET_FILE"
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Cleanup completed, script exiting" >> "$LOG_FILE"
        exit 0
    else
        # PID file exists but process is dead - clean up
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] WARNING: PID file found but process $FFMPEG_PID is not running - cleaning up stale files" >> "$LOG_FILE"
        rm -f "$PID_FILE" "$INFO_FILE" "$PRESET_FILE"
    fi
fi

# No recording running - START new recording based on preset
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

# Get active monitor info (FIXED: no longer uses pipe subshell)
get_active_monitor() {
    eval $(xdotool getmouselocation --shell)
    MOUSE_X=$X
    MOUSE_Y=$Y

    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Detecting active monitor - Mouse at: $MOUSE_X,$MOUSE_Y" >> "$LOG_FILE"

    # Store monitors in a variable instead of using pipe (fixes subshell return bug)
    local monitors=$(xrandr --current | grep " connected")

    while IFS= read -r line; do
        if [[ $line =~ ([0-9]+)x([0-9]+)\+([0-9]+)\+([0-9]+) ]]; then
            WIDTH="${BASH_REMATCH[1]}"
            HEIGHT="${BASH_REMATCH[2]}"
            OFFSET_X="${BASH_REMATCH[3]}"
            OFFSET_Y="${BASH_REMATCH[4]}"
            MAX_X=$((OFFSET_X + WIDTH))
            MAX_Y=$((OFFSET_Y + HEIGHT))

            if [ "$MOUSE_X" -ge "$OFFSET_X" ] && [ "$MOUSE_X" -le "$MAX_X" ] && \
               [ "$MOUSE_Y" -ge "$OFFSET_Y" ] && [ "$MOUSE_Y" -le "$MAX_Y" ]; then
                echo "[$(date '+%Y-%m-%d %H:%M:%S')] Found monitor: ${WIDTH}x${HEIGHT}+${OFFSET_X}+${OFFSET_Y}" >> "$LOG_FILE"
                echo "$WIDTH $HEIGHT $OFFSET_X $OFFSET_Y"
                return 0
            fi
        fi
    done <<< "$monitors"  # Use here-string instead of pipe

    echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: No monitor found containing mouse position" >> "$LOG_FILE"
    return 1
}

# Function to start recording based on preset
start_recording() {
    local preset=$1
    local width=$2
    local height=$3
    local offset_x=$4
    local offset_y=$5
    local output_path=$6

    case "$preset" in
        product)
            # PRODUCT DEMO: High quality, compact, smooth (current default)
            # Using veryfast preset for real-time capture without frame dropping
            notify-send "🔴 RECORDING STARTED" "Product Demo (30fps)\nPress Mod+r to stop" -i camera-video -u normal
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting product preset recording: ${width}x${height}+${offset_x}+${offset_y}" >> "$LOG_FILE"
            DISPLAY=:0 ffmpeg -f x11grab \
                -video_size "${width}x${height}" \
                -framerate 30 \
                -i ":0.0+${offset_x},${offset_y}" \
                -vf "fps=30,scale=1920:-1:flags=lanczos,format=yuv420p" \
                -c:v libx264 \
                -profile:v main \
                -level 4.0 \
                -preset veryfast \
                -tune zerolatency \
                -crf 20 \
                -g 60 \
                -movflags +faststart \
                -pix_fmt yuv420p \
                -y "$output_path" \
                >> "$LOG_FILE" 2>&1 &
            ;;

        tutorial)
            # TUTORIAL: 60fps, highest quality for detailed walkthroughs
            # Using fast preset because 60fps requires real-time encoding
            notify-send "🔴 RECORDING STARTED" "Tutorial (60fps HQ)\nPress Mod+Shift+r to stop" -i camera-video -u normal
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting tutorial preset recording: ${width}x${height}+${offset_x}+${offset_y}" >> "$LOG_FILE"
            DISPLAY=:0 ffmpeg -f x11grab \
                -video_size "${width}x${height}" \
                -framerate 60 \
                -i ":0.0+${offset_x},${offset_y}" \
                -vf "fps=60,scale=1920:-1:flags=lanczos,format=yuv420p" \
                -c:v libx264 \
                -profile:v high \
                -level 4.2 \
                -preset fast \
                -tune zerolatency \
                -crf 18 \
                -g 120 \
                -movflags +faststart \
                -pix_fmt yuv420p \
                -y "$output_path" \
                >> "$LOG_FILE" 2>&1 &
            ;;

        bug)
            # BUG REPORT: Fast, compact, good enough for bug reports
            notify-send "🔴 RECORDING STARTED" "Bug Report (compact)\nPress Mod+Alt+r to stop" -i camera-video -u normal
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting bug preset recording: ${width}x${height}+${offset_x}+${offset_y}" >> "$LOG_FILE"
            DISPLAY=:0 ffmpeg -f x11grab \
                -video_size "${width}x${height}" \
                -framerate 30 \
                -i ":0.0+${offset_x},${offset_y}" \
                -vf "fps=30,scale=1280:-1:flags=fast_bilinear,format=yuv420p" \
                -c:v libx264 \
                -profile:v baseline \
                -level 3.0 \
                -preset ultrafast \
                -tune zerolatency \
                -crf 26 \
                -g 60 \
                -movflags +faststart \
                -pix_fmt yuv420p \
                -y "$output_path" \
                >> "$LOG_FILE" 2>&1 &
            ;;
    esac

    echo $!
}

# Handle different presets
case "$PRESET" in
    gif)
        # QUICK GIF: 10 seconds auto-stop, optimized for GIFs
        OUTPUT_NAME="gif_${TIMESTAMP}.mp4"
        TEMP_OUTPUT="$RECORDINGS_DIR/$OUTPUT_NAME"
        FINAL_GIF="$GIFS_DIR/gif_${TIMESTAMP}.gif"

        MONITOR_INFO=$(get_active_monitor)
        if [ -z "$MONITOR_INFO" ]; then
            notify-send "Screen Recording" "Failed to detect active monitor" -i dialog-error
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: Failed to detect active monitor for gif preset" >> "$LOG_FILE"
            exit 1
        fi
        read WIDTH HEIGHT OFFSET_X OFFSET_Y <<< "$MONITOR_INFO"

        notify-send "🔴 GIF RECORDING" "10 seconds auto-capture...\nWill convert to GIF automatically" -i camera-video -u normal
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting gif preset recording: ${WIDTH}x${HEIGHT}+${OFFSET_X}+${OFFSET_Y}" >> "$LOG_FILE"

        # Record 10 seconds
        DISPLAY=:0 ffmpeg -f x11grab \
            -video_size "${WIDTH}x${HEIGHT}" \
            -framerate 15 \
            -t 10 \
            -i ":0.0+${OFFSET_X},${OFFSET_Y}" \
            -vf "fps=15,scale=1024:-1:flags=lanczos" \
            -c:v libx264 \
            -preset veryfast \
            -crf 23 \
            -y "$TEMP_OUTPUT" \
            >> "$LOG_FILE" 2>&1

        # Convert to optimized GIF
        if [ -f "$TEMP_OUTPUT" ]; then
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] Converting to GIF: $FINAL_GIF" >> "$LOG_FILE"
            ffmpeg -i "$TEMP_OUTPUT" \
                -vf "fps=15,scale=1024:-1:flags=lanczos,split[s0][s1];[s0]palettegen=max_colors=128[p];[s1][p]paletteuse=dither=bayer:bayer_scale=5" \
                -y "$FINAL_GIF" \
                >> "$LOG_FILE" 2>&1

            rm -f "$TEMP_OUTPUT"

            if [ -f "$FINAL_GIF" ]; then
                SIZE=$(du -h "$FINAL_GIF" | cut -f1)
                notify-send "✅ GIF COMPLETE" "Size: $SIZE\n$(basename $FINAL_GIF)" -i camera-video -u normal
                echo "[$(date '+%Y-%m-%d %H:%M:%S')] GIF created successfully: $FINAL_GIF ($SIZE)" >> "$LOG_FILE"
                echo "GIF saved: $FINAL_GIF"
            else
                notify-send "GIF Creation Failed" "Could not create GIF" -i dialog-error
                echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: GIF creation failed" >> "$LOG_FILE"
            fi
        else
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: Temporary MP4 file not created" >> "$LOG_FILE"
        fi
        exit 0
        ;;

    area)
        # AREA SELECTION: Use slop to select area
        if ! command -v slop &> /dev/null; then
            notify-send "Area Selection" "Error: slop not installed\nInstall with: paru -S slop" -i dialog-error
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: slop not installed" >> "$LOG_FILE"
            exit 1
        fi

        OUTPUT_NAME="area_${TIMESTAMP}.mp4"
        OUTPUT_PATH="$RECORDINGS_DIR/$OUTPUT_NAME"

        notify-send "Area Selection" "Click and drag to select recording area" -i camera-video
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Waiting for area selection..." >> "$LOG_FILE"

        # Get area selection
        AREA=$(slop -f "%w %h %x %y")
        if [ -z "$AREA" ]; then
            notify-send "Area Selection" "Cancelled" -i dialog-information
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] Area selection cancelled by user" >> "$LOG_FILE"
            exit 0
        fi

        read WIDTH HEIGHT OFFSET_X OFFSET_Y <<< "$AREA"

        notify-send "🔴 RECORDING STARTED" "Area selection (30fps)\nPress Mod+Ctrl+Shift+r to stop" -i camera-video -u normal
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Starting area preset recording: ${WIDTH}x${HEIGHT}+${OFFSET_X}+${OFFSET_Y}" >> "$LOG_FILE"

        DISPLAY=:0 ffmpeg -f x11grab \
            -video_size "${WIDTH}x${HEIGHT}" \
            -framerate 30 \
            -i ":0.0+${OFFSET_X},${OFFSET_Y}" \
            -vf "fps=30,format=yuv420p" \
            -c:v libx264 \
            -profile:v main \
            -level 4.0 \
            -preset veryfast \
            -tune zerolatency \
            -crf 20 \
            -g 60 \
            -movflags +faststart \
            -pix_fmt yuv420p \
            -y "$OUTPUT_PATH" \
            >> "$LOG_FILE" 2>&1 &

        FFMPEG_PID=$!
        echo "$FFMPEG_PID" > "$PID_FILE"
        echo "$OUTPUT_PATH" > "$INFO_FILE"
        echo "area" > "$PRESET_FILE"
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Area recording started with PID: $FFMPEG_PID" >> "$LOG_FILE"
        exit 0
        ;;

    product|tutorial|bug)
        # Standard presets: product, tutorial, bug
        OUTPUT_NAME="${PRESET}_${TIMESTAMP}.mp4"
        OUTPUT_PATH="$RECORDINGS_DIR/$OUTPUT_NAME"

        MONITOR_INFO=$(get_active_monitor)
        if [ -z "$MONITOR_INFO" ]; then
            notify-send "Screen Recording" "Failed to detect active monitor" -i dialog-error
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: Failed to detect active monitor for $PRESET preset" >> "$LOG_FILE"
            exit 1
        fi
        read WIDTH HEIGHT OFFSET_X OFFSET_Y <<< "$MONITOR_INFO"

        FFMPEG_PID=$(start_recording "$PRESET" "$WIDTH" "$HEIGHT" "$OFFSET_X" "$OFFSET_Y" "$OUTPUT_PATH")

        echo "$FFMPEG_PID" > "$PID_FILE"
        echo "$OUTPUT_PATH" > "$INFO_FILE"
        echo "$PRESET" > "$PRESET_FILE"

        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Recording started - Preset: $PRESET, PID: $FFMPEG_PID, Output: $OUTPUT_PATH" >> "$LOG_FILE"
        echo "Recording started with PID: $FFMPEG_PID"
        echo "Preset: $PRESET"
        echo "Output: $OUTPUT_PATH"
        ;;

    *)
        notify-send "Screen Recording" "Unknown preset: $PRESET\nValid: product, tutorial, gif, bug, area" -i dialog-error
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: Unknown preset: $PRESET" >> "$LOG_FILE"
        exit 1
        ;;
esac

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Script completed successfully" >> "$LOG_FILE"
