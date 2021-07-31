# inflex-server

stack build --file-watch  --exec 'cron-daemon --terminate --pid .stack-work/pid -ePORT=3031 -eCONFIG=config.yaml inflex-server' --flag inflex-server:postgresql --fast

ngrok http -subdomain=inflex 3031

Run tests

    stack test inflex-server

Fast run:

    stack exec -- ghcid -C test Spec.hs -Tmain --color=always

Connect to production database:

1. Enable inflex-pg-forward in Emacs via prodigy.

2. Connect to postgresql like this:

    psql -U doadmin -ddefaultdb -h localhost -p 54322
