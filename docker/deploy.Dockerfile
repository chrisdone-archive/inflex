FROM registry.gitlab.com/sky-above/inflex/release@sha256:...

ADD build /build
RUN mv /build/* /sbin/
