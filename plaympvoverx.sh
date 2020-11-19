#!/bin/bash

# Set pulse server to the connecting client's pulse server ip
export PULSE_SERVER=$(echo $SSH_CLIENT | awk '{print $1}')
WIDTH=$(ffprobe -v error -select_streams v:0 -show_entries stream=width -of csv=s=x:p=0 "$1")
HEIGHT=$(ffprobe -v error -select_streams v:0 -show_entries stream=height -of csv=s=x:p=0 "$1")
# compress to desired format and play

echo "$WIDTH"
echo "$HEIGHT"

NEWWIDTH=$(($WIDTH/2))
NEWHEIGHT=$(($HEIGHT/2))

echo "$NEWWIDTH"
echo "$NEWHEIGHT"

ffmpeg -i "$1" -preset veryfast -s ${NEWWIDTH}x${NEWHEIGHT} -crf 18 -f matroska - | mpv -

# reset terminal after play
reset
