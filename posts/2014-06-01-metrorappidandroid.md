---
title: MetroRappid for Android
tags: android atx hack mobile capmetro
---

## This app is now defunct! See the [Metrorappid web app][webapp]!

I participated in the [ATX Hack for Change][hfc] hackathon this weekend as part
of a great team, known in more educated circles as the HackStreetBoyz (my
initials *are* JT, you know.) We threw together a Google Maps-based Android
app for using Austin's MetroRapid bus and rail system.

Here's the presentation we showed Sunday evening.

<iframe
src="https://docs.google.com/presentation/d/1ibSV7R3iuHEUy8y9oRkZpP21lR_euqAewKTgZeC6PrQ/embed?start=false&loop=false&delayms=3000"
frameborder="0" width="640" height="480" allowfullscreen="true"
mozallowfullscreen="true" webkitallowfullscreen="true"></iframe>


## The Problem

CapMetro's app sucks. It's hard to use, it takes forever, it's enormous, it's
confusing, and it presents information in a really opaque way that doesn't line
up with how most people want to use it.

## The App

To solve this problem, we designed an Android app that combines the sleekness
and usability of Google Maps, with the real-time data that previously could
only be found in CapMetro’s own product

**First,** our teammate, [Luqmaan][luq], sniffed traffic from the original app
to expose the API calls that bring down real-time data about the 801 MetroRapid
and 550 MetroRail routes.

We [documented][docs] those... loosely, and put up some [sample data][data] for
testing.

**Next** we split into two groups, with Sean and Eric working on the Google
Maps API V2 integration while Luqmaan, Seth, and I tried to figure out how to
get [`Retrofit`][retrofit] to deserialize the XML responses from CapMetro.
``Retrofit`` is based on [``Simple``][simple], which uses some pretty neat
tricks (When did Java get decorators?) to automagically instantiate models from
XML.

**Finally**, Luq and I created some sexy icons and we all worked on wiring up
the map with annotations and markers from the CapMetro data.

There were, of course, a few iterations on the UI-side before we settled on a
choice but overall, getting the look and feel how we wanted it was probably the
easiest part.

The final MetroRappid app is essentially a single screen map view that
intelligently chooses an appropriate bus stop to display information based
on your current location.

#### It works like this:

  1. Open the app
  2. Walk towards the blue dot before time runs out and you miss your bus

![This is the first and only screen.][img_map]

In the end we built a workable app that I think we're all pretty happy with.
It's *way* faster and easier to use than the official CapMetro app, and only
crashes when you're trying to show it to someone, so that's something.

[img_map]: /images/metrorappid_map_small.jpg
[hfc]: 2014-06-01-atxhackforchange.html
[docs]: https://github.com/luqmaan/MetroRappid/wiki/The-CapMetro-API
[luq]: https://github.com/luqmaan
[data]: https://github.com/luqmaan/MetroRappidData
[retrofit]: http://square.github.io/retrofit/
[simple]: http://simple.sourceforge.net/
[webapp]: 2014-07-20-metrorappidwebapp.html
