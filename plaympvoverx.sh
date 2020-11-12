#!/bin/bash

export PULSE_SERVER=192.168.1.110
WIDTH=$(mediainfo "$1" | grep Width | awk -F':' '{print $2}' | sed -E 's/[a-zA-Z ]*([0-9]+).*/\1/g')
HEIGHT=$(mediainfo "$1" | grep Height | awk -F':' '{print $2}' | sed -E 's/[a-zA-Z ]*([0-9]+).*/\1/g')
#ffmpeg -i "$1" -preset veryfast -s 1280x720 -crf 23 -f matroska - | mpv -
#ffmpeg -i "$1" -preset veryfast -s 960x540 -crf 23 -f matroska - | mpv -
ffmpeg -i "$1" -preset veryfast -s "$WIDTH"x"$HEIGHT" -crf 18 -f matroska - | mpv -
