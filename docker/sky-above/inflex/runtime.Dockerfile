FROM fpco/pid1:18.04@sha256:f9deec3b086faa6a7a66f3a06a3dcd53315f29d688e36184f8e0d6cdd88985e4

RUN apt-get update && \
    apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y -qq \
            netbase ca-certificates libgmp-dev libz-dev libicu-dev libpq-dev libtinfo-dev
