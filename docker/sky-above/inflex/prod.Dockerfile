FROM registry.gitlab.com/sky-above/inflex/patch:366887100

COPY . /inflex
WORKDIR /inflex

# RUN mv /psc-package-output /inflex/inflex-client/output

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
    --flag inflex-server:release \
    --copy-bins \
    --local-bin-path=/bin/

FROM registry.gitlab.com/sky-above/inflex/runtime:2020-09-13

COPY --from=0 /bin/inflex-server /bin/inflex-server

RUN date

CMD ["/bin/inflex-server"]
