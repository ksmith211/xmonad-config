#!/bin/bash
amixer -q set Master 5%- unmute;
canberra-gtk-play -i audio-volume-change -d "changeVolume";
