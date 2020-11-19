#! /bin/bash
killall stalonetray
kill -9 $(ps -ax | grep applet | awk '{print $1}')
stalonetray 2>&1 & 
nm-applet 2>&1 &
blueman-applet 2>&1 &
