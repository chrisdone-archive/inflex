FROM registry.gitlab.com/sky-above/inflex/patch:189312240

COPY . /inflex
WORKDIR /inflex

RUN stack build inflex-shared

WORKDIR /inflex/inflex-client

RUN stack build \
    inflex-client \
    --exec ./bundle-full.sh

WORKDIR /inflex/inflex-server

RUN stack build \
    inflex-server \
    --flag inflex-server:postgresql \
    --flag inflex-server:release \
    --copy-bins \
    --local-bin-path=/bin/
