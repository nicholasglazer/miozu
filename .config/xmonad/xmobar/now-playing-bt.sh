#!/bin/bash

# Check if playerctl is available
if ! command -v playerctl >/dev/null; then
    echo "playerctl is not installed"
    exit 1
fi

# Get the current track metadata
artist="$(playerctl metadata artist 2>/dev/null)"
title="$(playerctl metadata title 2>/dev/null)"
current_status="$(playerctl status 2>/dev/null)"

# Check if the metadata is available and the status is "Playing"
if [ -z "$artist" ] || [ -z "$title" ] || [ "$current_status" != "Playing" ]; then
    if [ "$current_status" == "Playing" ]; then
        echo "No track is currently playing"
    else
        echo "Waiting for next track..."
    fi
    exit 0 # Exit with 0 instead of 1 to indicate success but no output
fi

# Define the maximum number of characters for artist and title
max_chars=28

# Check if the artist or title exceed the maximum number of characters
if [ ${#artist} -gt $max_chars ]; then
    artist="${artist:0:$((max_chars))}..." # Add ellipsis at the end of artist
fi

if [ ${#title} -gt $max_chars ]; then
    title="...${title: -$((max_chars + 22))}" # Add ellipsis at the beginning of title
fi

# Display the current track metadata with ellipsis if needed
output=""
if [ -n "$artist" ] && [ -n "$title" ]; then
    output="${artist} - ${title}"
fi

echo "$output"
