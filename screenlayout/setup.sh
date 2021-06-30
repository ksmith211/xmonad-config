#!/bin/sh
xrandr --output DVI-D-0 --primary --mode 1920x1200 --pos 0x0 --rotate normal --left-of HDMI-0
xrandr --output HDMI-0 --mode 1920x1080 --right-of DVI-D-0 

feh --bg-fill ~/.screenlayout/mars-curiosity.jpg
nohup redshift -c $HOME/.config/redshift.conf 2>&1 &
nohup get-volume.sh 2>&1 &
nohup get-brightness.sh 2>&1 &
nohup bitcoin_bpi.sh 2>&1 &

nohup systemctl --user start dunst
nohup xmobar -x 1 .xmobarrc2 2>&1 &
