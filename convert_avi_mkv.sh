#! /bin/bash

ffmpeg -fflags +genpts -i "$1" -vcodec copy -acodec copy "$1.mkv"
