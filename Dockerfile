FROM fpco/alpine-haskell-stack:8.6.5

RUN adduser -u 1000 chris --disabled-password && \
    install -d -m 0755 -o chris -g chris /home/chris

ARG USER_ID=1000
ARG GROUP_ID=1000

RUN apk update && apk add sdl2 sqlite sdl2-dev sqlite-dev ncurses-dev ncurses-static

USER chris
