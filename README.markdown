# rss-markdown-proxy

> A reverse proxy rendering Markdown to HTML within RSS feeds for podcast
> shownotes.

## Why?

As my co-host [@monchote](https://github.com/monchote) on
[Strictly Untyped](https://twitter.com/strictlyuntyped) has found out,
SoundCloud does not allow embedding HTML in their RSS feeds because
spammers could take advantage of this. This is, however, pretty crap
if you want to use it for show notes and embed some links.

This is a simple reverse proxy you can pipe any RSS feed through
to render the `<description>` and `<itunes:summary>` fields to HTML, allowing
you to author them in Markdown.

## Setup

I will provide some pre-built binaries as soon as this is Stable Enough™.

```
$ git clone http://github.com/passy/rss-markdown-proxy
$ cd rss-markdown-proxy
$ stack setup
$ stack install
$ rss-markdown-proxy "http://feeds.soundcloud.com/users/soundcloud:users:189413584/sounds.rss"
$ curl "http://localhost:3000/feed.rss"
```

N.B. Right now, the command line argument is actually ignored. I'll
take care of that later.

## Caching

Responses are cached for five minutes by default.

## License

BSD-3
