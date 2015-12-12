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
