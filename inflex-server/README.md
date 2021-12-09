# inflex-server

## Dev

docker run -ePOSTGRES_USER=inflex -ePOSTGRES_PASSWORD=inflex -ePOSTGRES_DB=inflex --name skyabove-pg -p 54321:5432 postgres:12-alpine@sha256:3524b51b7929263b2ac8e5bc117a14ff00802f71c81dff78a2a9268ca4f27214

stack build --file-watch  --exec 'cron-daemon --terminate --pid .stack-work/pid -ePORT=3031 -eCONFIG=config.yaml inflex-server' --flag inflex-server:postgresql --fast

## Dump prod schema

docker run --net=host -i postgres:12-alpine pg_dump -U doadmin -ddefaultdb -h localhost -p 54322 --no-owner --schema-only --no-privileges > ~/Work/skyabove/inflex/inflex-server/schema.sql

## Import prod schema

docker run --net=host -v$HOME/Work/skyabove/inflex/inflex-server:/w -i postgres:12-alpine psql -U inflex -h localhost -d inflex -p 54321 -f /w/schema.sql

## ngrok

ngrok http -subdomain=inflex 3031

## Tests

Run tests

    stack test inflex-server

Fast run:

    stack exec -- ghcid -C test Spec.hs -Tmain --color=always

## Prod DB

Connect to production database:

1. Enable inflex-pg-forward in Emacs via prodigy.

2. Connect to postgresql like this:

    psql -U doadmin -ddefaultdb -h localhost -p 54322

via docker:

    docker run --net=host -i postgres:12-alpine psql -U doadmin -ddefaultdb -h localhost -p 54322
