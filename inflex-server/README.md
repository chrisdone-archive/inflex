# inflex-server

stack build --file-watch  --exec 'cron-daemon --terminate --pid .stack-work/pid -ePORT=3031 -eCONFIG=config.yaml inflex-server' --flag inflex-server:postgresql --fast

ngrok http -subdomain=inflex 3031

Run tests

    stack test inflex-server

Fast run:

    stack exec -- ghcid -C test Spec.hs -Tmain --color=always
