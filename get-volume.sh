# ~/bin/get-volume.sh
#!/bin/bash
# Get the maximum volume of any pulseaudio sink channel
# amixer get Master | egrep -o "[0-9]+%"
while true; do
  vol=$(pulsemixer --get-volume | awk '{print $1}')
  echo "Vol: $vol%" > $HOME/.sysinfo/volume
  echo "Vol: $vol%" > $HOME/.sysinfo/volume2
  sleep 1
done
exit 0
