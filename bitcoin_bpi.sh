# ~/bin/get-volume.sh
#!/bin/bash
# Get the maximum volume of any pulseaudio sink channel
# amixer get Master | egrep -o "[0-9]+%"
while true; do
  btc=$(curl -s https://api.coindesk.com/v1/bpi/currentprice.json | jq '.bpi.USD.rate')
  echo "BTC: $btc" > $HOME/.sysinfo/bitcoin
  sleep 60 
done
exit 0
