#!/bin/bash

# Set pulse server to the connecting client's pulse server ip
export PULSE_SERVER=$(echo $SSH_CLIENT | awk '{print $1}')

# compress to desired format and play
ffmpeg -i "$1" -preset veryfast -s 960x540 -crf 18 -f matroska - | mpv -

# reset terminal after play
reset
