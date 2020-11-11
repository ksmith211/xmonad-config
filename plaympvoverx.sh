#!/bin/bash

export PULSE_SERVER=192.168.1.110
ffmpeg -i "$1" -preset veryfast -s 1280x720 -crf 18 -f matroska - | mpv -
