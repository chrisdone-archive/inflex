FROM debian:9-slim@sha256:1a470b92197dd16a46f7aa9cb308fa91f7d0948e0dccd625a03cbbdf2d4516e6
MAINTAINER Chris Done

################################################################################
# Haskell system dependencies (basically never changes)

RUN apt-get update && \
    apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y -qq \
            netbase git ca-certificates xz-utils build-essential curl unzip libgmp-dev

################################################################################
# Download a specific Stack version

RUN curl https://github.com/commercialhaskell/stack/releases/download/v2.3.0.1/stack-2.3.0.1-linux-x86_64-static.tar.gz \
    --silent -L \
    -o stack.tar.gz && \
    tar zxf stack.tar.gz && mv stack-2.3.0.1-linux-x86_64-static/stack /usr/bin/

################################################################################
# Switch to work dir

ADD stack.yaml /build-workdir/stack.yaml
ADD stack.yaml.lock /build-workdir/stack.yaml.lock
ADD inflex-server /build-workdir/inflex-server
ADD inflex-engine /build-workdir/inflex-engine
ADD inflex-client /build-workdir/inflex-client
WORKDIR /build-workdir

################################################################################
# Install the right GHC version and update package index

RUN pwd && stack setup && stack update

################################################################################
# Install the snapshot and system dependencies

RUN apt-get install -y libz-dev libicu-dev libtinfo-dev libpq-dev
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y locales
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=en_US.UTF-8
ENV LANG en_US.UTF-8

RUN stack build --only-snapshot --test --no-run-tests
WORKDIR /
RUN rm -rf /build-workdir
