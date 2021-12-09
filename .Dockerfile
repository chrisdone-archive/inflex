FROM registry.gitlab.com/sky-above/inflex/patch:2021-11-24

ARG USER_ID=1000
ARG GROUP_ID=1000

RUN if getent group chris ; then groupdel chris; fi &&\
    groupadd -g ${GROUP_ID} chris &&\
    useradd -l -u ${USER_ID} -g chris chris &&\
    install -d -m 0755 -o chris -g chris /home/chris &&\
    chown -R chris:chris /root/.stack

COPY .Dockerfile-bashrc /home/chris/.bashrc
RUN chown chris:chris /root/

USER chris

ENV STACK_ROOT="/root/.stack/"
ENV PATH="/root/.stack/bin:${PATH}"
