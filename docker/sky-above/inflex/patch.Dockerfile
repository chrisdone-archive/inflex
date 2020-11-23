FROM registry.gitlab.com/sky-above/inflex/base:2020-09-13@sha256:45dfc53d2bfb392db0fe4b08b2d572bf5669cf730ea7f6958c78f2bbffe798d7

COPY . /inflex
WORKDIR /inflex

WORKDIR /inflex/inflex-client

RUN stack exec -- psc-package build --only-dependencies && \
    cp -r output/ /psc-package-output

WORKDIR /inflex/inflex-server

RUN stack build \
    --only-snapshot \
    --flag inflex-server:postgresql \
    --flag inflex-server:release \
    --test \
    --no-run-tests
