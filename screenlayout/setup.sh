#!/bin/sh
# xrandr --output LVDS-1  --primary --mode 1280x800 --pos 0x0 --rotate normal 
# xrandr --output HDMI-1 --primary --mode 1920x1200 --pos 0x0 --rotate normal

# below is to get second display over thunderbolt to work
#xrandr --output HDMI-1 --mode 1920x1080 --left-of LVDS-1
# xrandr -s 0 # reset displays
nohup feh --bg-fill ~/.screenlayout/mars-curiosity.jpg 2>&1 &
#redshift -o -P -l 42:-73 -b 1.0 -O 3500
# see ~/.config/redshift.conf for configuration
nohup killall redshift 2>&1 &
nohup redshift -c $HOME/.config/redshift.conf 2>&1 &
nohup sleep 10 2>&1 &
nohup xmobar2 
#get-volume.sh 2>&1 &
#get-brightness.sh 2>&1 &
nohup bitcoin_bpi.sh 2>&1 &
nohup stalone.sh 2>&1 &
