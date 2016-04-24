# rss-markdown-proxy
[![Build Status](https://travis-ci.org/passy/rss-markdown-proxy.svg)](https://travis-ci.org/passy/rss-markdown-proxy)

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

```
$ git clone http://github.com/passy/rss-markdown-proxy
$ cd rss-markdown-proxy
$ stack setup
$ stack install
```

## Usage

This package comes with two binaries: `rss-markdown-proxy` and
`rss-markdown-proxy-server`. The former is a stand-alone CLI tool version
of the latter.

### `rss-markdown-proxy-server`

This starts a web server for a given URL and serves the transformed feed at
`/feed.rss`. Responses are cached for 5 minutes. Open an issue if you believe
that this should be configurable. It binds to "0.0.0.0". Again, open an issue or
PR if that bothers you.

Example usage:

```bash
$ rss-markdown-proxy-server -p 3000 "https://feeds.soundcloud.com/users/soundcloud:users:189413584/sounds.rss"
$ curl http://localhost:3000/
```

### `rss-markdown-proxy`

This tool can be used to transform local or remote feeds one-off or as part
os a scheduled job.

Local file:

```bash
# From the filesystem
$ rss-markdown-proxy test/fixtures/sounds.rss
# From STDIN
$ rss-markdown-proxy < test/fixtures/sounds.rss
# From URL
$ rss-markdown-proxy -u "https://feeds.soundcloud.com/users/soundcloud:users:189413584/sounds.rss"
```

## Metrics

Like stats? So do I! If you compile with the `metrics` flag, you get a built-in
metrics server. Only with `rss-markdown-proxy-server`, though, as it doesn't
make a whole lot of sense for short-lived programs.

```
$ stack build --flag=rss-markdown-proxy:metrics
```

The metrics server runs on port 3001 by default but you can change it through
the `--metrics-port` / `-m` flags. The server binds to localhost and you
probably don't want to expose this directly to the internet. Either
reverse-proxy it with some authentication or SSH tunnel to it.

## Security Considerations

The proxy is built to only serve a single URL. You could easily change it
to proxy arbitrary RSS feeds, but I strongly advise against that setup.
XML is a horrible mess and a lot of the code runs in IO. I have no idea
whether or not this is vulnerable to the
[Billion laughs](https://en.wikipedia.org/wiki/Billion_laughs) attack
so I'd rather be safe then sorry by limiting this to trusted sources.

## Dockerizificationism :whale:

If you want to use Docker to deploy this somewhere, here are some steps
which might be useful. You need a recent version of Docker that supports
the `ARG` command if you want to build an image yourself. There
are (semi-)automatic builds available on
[Docker Hub](https://hub.docker.com/r/passy/rss-markdown-proxy/).

*Run image*

```
docker pull rss-markdown-proxy:v0.2.0.0
docker run --rm -p 3000:3000 rss-markdown-proxy:v0.2.0.0 "https://feeds.soundcloud.com/users/soundcloud:users:189413584/sounds.rss"
```

*Build a new image*

```
docker build --build-arg version=$(git describe --tags --always) -t rss-markdown-proxy .
```

*Release an image*

Check the version that's spat out above.

```
docker tag <image_id> passy/rss-markdown-proxy:<version_tag>
docker push passy/rss-markdown-proxy:<version_tag>
```

## Example

Input snippet:

```xml
<itunes:summary>For their first real episode, Ramón and Passy talk about some of the latest news in tech and address the elephant in the room: Android vs iOS from a DX (Developer Experience) perspective.

## Follow-Up

* Talking about podcasts on podcasts: https://overcast.fm/+EtBoIE-HU/7:21

## Other Topics

* Apple open sourcing Swift: https://github.com/apple/swift
* Dropbox shutting down Mailbox + Carousel: https://blogs.dropbox.com/dropbox/2015/12/saying-goodbye-to-carousel-and-mailbox/
* Apple's new gorgeous iPhone battery case: http://www.apple.com/shop/product/MGQM2LL/A/iphone-6s-smart-battery-case-white
* Apple May Replace 3.5mm Headphone Jack on iPhone 7 With All-in-One Lightning Connector: http://www.macrumors.com/2015/11/27/iphone-7-no-3-5mm-headphone-jack-lightning/
* Google Play Store will soon display if an app contains ads: http://www.techtimes.com/articles/108538/20151120/google-play-store-will-soon-warn-users-if-apps-have-ads.htm

## Links

* Android Needs A Simulator by Jake Wharton: http://jakewharton.com/android-needs-a-simulator/

## About

* Ramón Argüello: https://twitter.com/monchote
* Pascal Hartig: https://twitter.com/passy
* Strictly Untyped: https://twitter.com/strictlyuntyped
</itunes:summary>
```

Output snippet:

```xml
<itunes:summary>
  <![CDATA[
  <p>For their first real episode, Ramón and Passy talk about some of the latest
  news in tech and address the elephant in the room: Android vs iOS from a DX
  (Developer Experience) perspective.</p> <h2>Follow-Up</h2> <ul> <li>
  <p>Talking about podcasts on podcasts: https://overcast.fm/+EtBoIE-HU/7:21</p>
  </li> </ul><h2>Other Topics</h2><ul><li>Apple open sourcing Swift:
  https://github.com/apple/swift</li><li>Dropbox shutting down Mailbox +
  Carousel:
  https://blogs.dropbox.com/dropbox/2015/12/saying-goodbye-to-carousel-and-mailbox/</li><li>Apple's
  new gorgeous iPhone battery case:
  http://www.apple.com/shop/product/MGQM2LL/A/iphone-6s-smart-battery-case-white</li><li>Apple
  May Replace 3.5mm Headphone Jack on iPhone 7 With All-in-One Lightning
  Connector:
  http://www.macrumors.com/2015/11/27/iphone-7-no-3-5mm-headphone-jack-lightning/</li><li>Google
  Play Store will soon display if an app contains ads:
  http://www.techtimes.com/articles/108538/20151120/google-play-store-will-soon-warn-users-if-apps-have-ads.htm</li></ul><h2>Links</h2><ul><li><p>Android
  Needs A Simulator by Jake Wharton:
  http://jakewharton.com/android-needs-a-simulator/</p></li></ul><h2>About</h2><ul><li>Ramón
  Argüello: https://twitter.com/monchote</li><li>Pascal Hartig:
  https://twitter.com/passy</li><li>Strictly Untyped:
  https://twitter.com/strictlyuntyped</li></ul>]]>
</itunes:summary>
```

## License

BSD-3
