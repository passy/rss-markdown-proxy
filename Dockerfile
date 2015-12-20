FROM fpco/stack-run
MAINTAINER Pascal Hartig <phartig@rdrei.net>

ARG version=v0.1.0.0

RUN apt-get install curl && mkdir -p /srv
RUN curl -L https://github.com/passy/rss-markdown-proxy/releases/download/$version/rss-markdown-proxy-$version-lnx64.tar.bz2 | tar -C /srv -xjvf - rss-markdown-proxy-server

EXPOSE 3000
WORKDIR /srv
ENTRYPOINT ["/srv/rss-markdown-proxy-server"]

# vim:tw=0:
