#!/bin/bash

# Check if playerctl is available
if ! command -v playerctl >/dev/null; then
    echo "Playerctl is not installed"
    exit 1
fi

# Set the name of the file to store the previous track name and status
previous_track_file="$HOME/.tmp/previous_track.txt"

# Get the current track name and status
current_track="$(playerctl metadata artist) - $(playerctl metadata title)"
current_status="$(playerctl status)"

# Check if the metadata is available
if [ -z "$current_track" ]; then
    echo "No track is currently playing"
    exit 1
fi

# Get the name and status of the previous track if it's different from the current track
IFS=$'\n' read -d '' -r -a previous_track <<<"$(cat "$previous_track_file")"
previous_track_name="${previous_track[0]}"
previous_track_status="${previous_track[1]}"
if [ "$current_track" != "$previous_track_name" ]; then
    previous_track_name="$current_track"
    previous_track_status="$current_status"
    echo "$previous_track_name"$'\n'"$previous_track_status" >"$previous_track_file"
fi

# Display the current or previous track's title based on its status
if [ "$current_status" = "Paused" ]; then
    echo "Paused: $previous_track_name"
else
    echo "$current_track"
fi
