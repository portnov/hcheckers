FROM ubuntu
MAINTAINER Ilya V. Portnov <portnov84@rambler.ru>

RUN apt-get update -y && apt-get install -y libffi6 libgmp10

RUN mkdir -p /opt/hcheckers/{bin,etc}

ADD build/hcheckersd /opt/hcheckers/bin/hcheckersd
ADD hcheckersd.yaml /opt/hcheckers/etc/hcheckersd.yaml

EXPOSE 8864

VOLUME /opt/hcheckers/etc

CMD ["/opt/hcheckers/bin/hcheckersd", "-c", "/opt/hcheckers/etc/hcheckersd.yaml"]

