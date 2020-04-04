FROM registry.gitlab.com/sky-above/inflex/release@sha256:d8325deed6665357b0aa18ef06415f29decb8317c17e83f902ea4cc1ecca05b8

ADD build /build
RUN mv /build/* /sbin/
