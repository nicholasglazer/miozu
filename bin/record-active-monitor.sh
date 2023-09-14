#!/bin/bash

# Default values
name=""
timestamp=false
timestamp_format="%Y-%m-%d_%H:%M:%S"
extension=".gif"
fps=10
# To keep the aspect ratio, we need to specify only one component, either width or height, and set the other component to -1
scale="800:-1"
scaleflag="lanczos"
path="$HOME/Pictures/gifs/"
custom=""
duration=10

# Function to display usage instructions
instructions() {
    echo "Usage: $0 [OPTIONS]"
    echo "Capture screen using FFmpeg"
    echo
    echo "Options:"
    echo "  -n,   --name              Specify the output file name (default: %Y-%m-%d_%H:%M:%S)"
    echo "  -t,   --timestamp         Append a timestamp to the output file name (default: false || true if name is empty)"
    echo "  -tf,  --timestamp-format  Specify the timestamp format (default: %Y-%m-%d_%H:%M:%S)"
    echo "  -e,   --extension         Specify the output file extension (default: .gif)"
    echo "  -f,   --fps               Specify the frames per second (default: 10)"
    echo "  -sc,  --scale             Specify the scaling dimensions (default: 800:-1)"
    echo "  -scf, --scale-flag        Specify the scaling algorithm (default: lanczos)"
    echo "  -p,   --path              Specify the output directory (default: $HOME/Pictures/gifs/)"
    echo "  -d,   --duration          Specify the duration of the recording in seconds (default: 15)"
    echo "  -c,   --custom            Add custom FFmpeg properties"
    echo "  -h,   --help              Display this help message"
    echo
    exit 1
}
# Function to handle errors
handle_error() {
    local exit_code=$1
    local error_message=$2

    echo "Error: $error_message" >&2
    exit $exit_code
}

# Parse command-line options
while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        -n | --name)
            name="$2"
            shift
            shift
            ;;
        -t | --timestamp)
            timestamp=true
            shift
            ;;
        -tf | --timestamp-format)
            timestamp_format="$2"
            shift
            shift
            ;;
        -e | --extension)
            extension="$2"
            shift
            shift
            ;;
        -f | --fps)
            fps="$2"
            shift
            shift
            ;;
        -sc | --scale)
            scale="$2"
            shift
            shift
            ;;
        -scf | --scale-flag)
            scaleflag="$2"
            shift
            shift
            ;;
        -p | --path)
            path="$2"
            shift
            shift
            ;;
        -c | --custom)
            custom="$2"
            shift
            shift
            ;;
        -d | --duration)
            duration="$2"
            shift
            shift
            ;;
        -h | --help)
            instructions
            ;;
        *)
            handle_error 1 "Invalid option: $key"
            ;;
    esac
done

# Generate output file name
if [[ -z $name ]]; then
    name="$(date +"$timestamp_format")"
fi

if [[ -n $name && $timestamp == true ]]; then
    name="${name}_${timestamp_string}"
elif [[ -n $name && $timestamp == false ]]; then
    name="$name"
fi

# Discover the active monitor based on mouse position
#
# Extract the offset values of the active monitor from a string
ACTIVE_MONITOR_OFFSET_PATTERN="\+([-0-9]+)\+([-0-9]+)"

# Get the window position
eval "$(xdotool getmouselocation --shell)"

# Loop through each screen and compare the offset with the window coordinates.
monitor_index=0
while read -r active_monitor width height offsetX offsetY; do
    if [[ $X -ge $offsetX && $Y -ge $offsetY && $X -lt $((offsetX + width)) && $Y -lt $((offsetY + height)) ]]; then
        break
    fi
    ((monitor_index++))
done < <(xrandr | grep -w connected |
    sed -r "s/^([^ ]*).*\b([-0-9]+)x([-0-9]+)$ACTIVE_MONITOR_OFFSET_PATTERN.*$/\1 \2 \3 \4 \5/" |
    sort -nk4,5)

# If we found a monitor, echo it out; otherwise, print an error.
if [ ! -z "$active_monitor" ]; then
    # Might be useful to debug or to export, it will echo $monitor variables as serialized JSON
    echo "{ \"monitor_index\": $monitor_index, \"width\": $width, \"height\": $height, \"offsetX\": $offsetX, \"offsetY\": $offsetY, \"active_monitor\": \"$active_monitor\" }"
    # Construct FFmpeg command with active monitor variables and custom properties
    ffmpeg_command="/usr/bin/ffmpeg -f x11grab -video_size ${width}x${height} -t 15 -y -i :0.0+${offsetX},${offsetY} -vf fps=$fps,scale=$scale:$scaleflag $custom"
    $ffmpeg_command "$path$name$extension"
    exit 0
else
    echo "Couldn't find any monitor for the current window." >&2
    exit 1
fi
