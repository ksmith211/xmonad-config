# ~/bin/get-volume.sh
#!/bin/bash
# Get the maximum volume of any pulseaudio sink channel
# amixer get Master | egrep -o "[0-9]+%"
while true; do
  bright=$(xrandr --verbose | grep Brightness | sed -sn 1p)
  gamma=$(xrandr --verbose | grep Gamma | sed -sn 1p)
  echo "$bright $gamma" > $HOME/.sysinfo/brightness
  sleep 20
done
exit 0
