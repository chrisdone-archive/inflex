FROM registry.gitlab.com/sky-above/inflex/build@sha256:ea2a9e5b9b1b92e41e8b1ada0c062f72930b6f26c1bc0d3050e05948c5b4f3dd
ADD . /build-workdir
WORKDIR /build-workdir
RUN stack build psc-package && stack build --only-dependencies
WORKDIR /
RUN rm -rf /build-workdir
