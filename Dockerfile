FROM fpco/stack-run
MAINTAINER Pascal Hartig <phartig@rdrei.net>

RUN apt-get install curl && mkdir -p /srv
RUN curl https://github.com/passy/rss-markdown-proxy/releases/download/v0.1.0.0/rss-markdown-proxy-v0.1.0.0-lnx64.tar.bz2 | tar -C /srv -xzvf - rss-markdown-proxy-server

EXPOSE 3000
WORKDIR /srv

ENTRYPOINT rss-markdown-proxy-server
