FROM fpco/stack-run
MAINTAINER Pascal Hartig <phartig@rdrei.net>

ARG PROGVERSION=v0.2.0.0

RUN apt-get install -y curl && mkdir -p /srv
RUN curl -L https://github.com/passy/rss-markdown-proxy/releases/download/$PROGVERSION/rss-markdown-proxy-$PROGVERSION-lnx64.tar.bz2 | tar -C /srv -xjvf - ./rss-markdown-proxy-server

EXPOSE 3000
WORKDIR /srv
ENTRYPOINT ["/srv/rss-markdown-proxy-server"]

# vim:tw=0:
