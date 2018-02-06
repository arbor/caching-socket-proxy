FROM ubuntu:16.04
LABEL maintainer="Tyler Durden <mayhem@arbor.net>"

ADD docker/set-environment.sh /usr/local/bin/set-environment

RUN apt-get update && \
    apt-get install -y jq libgmp10 libssl1.0.0 iproute netbase ca-certificates && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY .librdkafka/ /usr/

ENV LOG_LEVEL LevelInfo

ADD docker/start.sh /start.sh
ADD docker/test.sh /test.sh
ADD .stack-work/install/x86_64-linux*/*/*/bin/* /usr/local/bin/

CMD ["/start.sh"]
