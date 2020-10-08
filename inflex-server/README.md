# inflex-server

stack build --file-watch inflex-server --exec 'cron-daemon --terminate --pid .stack-work/pid -ePORT=3031 -eCONFIG=../config.yaml inflex-server' --fast


ngrok http -subdomain=inflex 3031
