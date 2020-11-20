#! /bin/bash

ffmpeg -i "$1" -preset veryfast -crf 18 -f matroska - | nc -l -p 8111
echo "Netcat serving video stream on port 8111"
echo "Type nc 192.168.1.4 8111 | mpv -"
echo "...and enjoy :)"
