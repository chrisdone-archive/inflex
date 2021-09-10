FROM registry.gitlab.com/sky-above/inflex/base:2020-09-13@sha256:45dfc53d2bfb392db0fe4b08b2d572bf5669cf730ea7f6958c78f2bbffe798d7

COPY . /inflex
WORKDIR /inflex

COPY . /inflex
WORKDIR /inflex

RUN date; stack build inflex-shared

WORKDIR /inflex/inflex-client

RUN date; stack build \
    purescript \
    inflex-client \
    --exec ./bundle-full.sh

WORKDIR /inflex/inflex-server

RUN date; stack build \
    inflex-server \
    --flag inflex-server:postgresql \
    --flag inflex-server:release
