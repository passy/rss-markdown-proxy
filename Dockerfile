FROM baseimage:0.9.15

RUN apt-get install curl
RUN curl -O https://github.com/passy/rss-markdown-proxy/releases/download/v0.1.0.0/rss-markdown-proxy-v0.1.0.0-lnx64.tar.bz2 | tar -xzvf - rss-markdown-proxy-server

CMD ["rss-markdown-proxy-server", "-p", "8000", "feeds.soundcloud.com/users/soundcloud:users:189413584/sounds.rss"]
