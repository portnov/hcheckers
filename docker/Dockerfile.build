FROM ubuntu
MAINTAINER Ilya V. Portnov <portnov84@rambler.ru>

RUN apt-get update -y && apt-get install -y ca-certificates curl
RUN apt-get install -y unzip zlib1g-dev c2hs pkg-config libreadline-dev
# Version of stack in ubuntu repos is too old.
RUN curl -sSL https://get.haskellstack.org/ | sh

RUN stack setup

WORKDIR /src
VOLUME /src/hcheckers-master/build
VOLUME /dst

ADD builder-entrypoint.sh /

CMD ["/bin/bash", "/builder-entrypoint.sh"]
