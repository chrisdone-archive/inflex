FROM registry.gitlab.com/sky-above/inflex/src:2020-09-13

WORKDIR /build-workdir

################################################################################
# Install the right GHC version and update package index

RUN pwd && stack setup && stack update

################################################################################
# Install the snapshot and system dependencies

RUN stack build --only-snapshot --test --no-run-tests
RUN stack build psc-package
WORKDIR /
RUN rm -rf /build-workdir
