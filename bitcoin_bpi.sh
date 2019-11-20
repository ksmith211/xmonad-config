# ~/bin/bitcoin_bpi.sh
#!/bin/bash
while true; do
  btc=$(curl -s https://api.coindesk.com/v1/bpi/currentprice.json | jq '.bpi.USD.rate')
  echo "BTC: $btc" > $HOME/.sysinfo/bitcoin
  sleep 60 
done
exit 0
