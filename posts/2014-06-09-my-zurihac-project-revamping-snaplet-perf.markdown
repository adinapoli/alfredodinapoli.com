---
title: My ZuriHac 2014 project: Revamping snaplet-perf
description: Description of my project at this ZuriHac
tags: haskell, snap, zurihac
---

#My ZuriHac 2014 project: Revamping snaplet-perf

[ZuriHac]() wrapped up yesterday, and needless to say, it was an amazing event.
Bas and the other guys at [Better]() really made an incredible job, and the
response from the community was just amazing. It was my first hackaton *ever*,
but according to the others this year ZuriHac had three times more people than
the last year, which is, I think, really promising.
Being my first hackaton, I wasn't sure about the rules, so I ended up choosing
a project thinking it was mandatory; in retrospective I regret not having just
jumped in and out from room to room, to meet even more people than I did.
Anyway, lesson learned! The project I choose was to revamp _snaplet-perf_.
In this small post (mostly written at the airport) I'll motivate my choices
and finally show you a brief demo.

## What's snaplet-perf?
If you are not living under a Haskell-y rock, you probably know that there
are at least three prominent web frameworks written in Haskell: Yesod, Snap
and Happstack (list not exhaustive!). Snap 0.6 onwards introduced the concept
of "Snaplets", self contained "piece of functionality" you can plug to your
application, in a sort of "off the shelf" fashion. There are many snaplets
available on Hackage, so why did I choose _snaplet-perf_?
When I first contributed to Snap one year and a half ago (here the
[full story]()) Doug Beardsley, one of the lead dev on Snap, gave me access
to a private repository (i.e. snaplet-perf) within the "Snap Framework"
organization on Github, so that I could contribute to the Snap ecosystem in
a sort of "low-pressure" environment. Snaplet-perf itself seems to be a sort
of "middleground" for new Snap contributors. Ok, but what it does exactly?
The idea behind snaplet-perf is that you plug it into your application, and
it will record the response time of all your endpoints, so that you can get
some insights on your data (.e.g. endpoint X is getting slower over time etc).

## What was there
When I joined the project, almost two years ago, what was there was just
a tiny middleware layer which was saving the response time of the endpoint
using a caching library called [zoom-cache](). The idea behind zoom-cache is
that allows you to "zoom" into intervals of your data, making easy to focus
on a particular period of your application lifetime. Seems neat, right?
Unfortunately it turned out that for some reason snaplet-perf was slowly
bit rotting, and here I'll try to explain what I think is the cause.

## What was my plan for the ZuriHac
zoom-cache turned out to be the elephant in the room. Talking with other
developers which joined the project, they admitted to have being put off
by the presence of zoom-cache. One week before the ZuriHac, I tried to
make snaplet-perf compile again under GHC 7.8.2 before the hackaton, but
several problems arose:

* zoom-cache was not compiling anymore, because it relies on a library
  called [type-level]() for "type level naturals".

* type-level hasn't been updated since 2010, it has a repository in darcs
  (making more difficult, at least for me, to send PR)

* type-level itself is full of type level programming and is heavily
  abusing functional dependencies in its type classes definition. Those
  type classes constrains were being rejected now in GHC 7.8.2, as being
  "not fully qualifying". This is a random example:

``` haskell
class Succ x y | x -> y, y -> x where
...
```

Note how it's basically expressing mutually recursive functional
dependencies.

## What I got at the end of Friday
So the plan was to shave the yak a bit, fix type-level, fix zoom-cache and
finally adding a nice UI on top of that, capable of showing real time graphs
of the whole data. But sadly, after a day of head banging, all my solutions
were going nowhere:

* Making type-level compile again was a no-go; we tried (Andres and Ollie
  tried to help me), but I concluded it was simply not worth it, at least
  without massive effort in rewriting part of the library.

* type-level itself uses Template Haskell to automatically generate this
  type level naturals up to 5000, so every application wanting to use any
  data structure with more than 5000 elements is simply constrained by this
  upper bound.

* Creating a small module within zoom-cache itself to mimic the type-level API
  seemed possible, but after several experiments with `GHC.TypeLits` and a
  massive doses of painkillers, the situation was not bright at all.

* zoom-cache depends upon the `iteratees` streaming library, which is fine
  because snap-server uses it as well. Starting from snap 1.0 though,
  the server will be 100% [io-streams]() powered, making somewhat unfortunate
  the need for two streaming libraries in the same dependency graph.

Considering all this points, and considering that it was an hackaton and that
I should really have fun with it, I have decided to simply drop zoom-cache
altogether and using [acid-state]() for the persistence layer. If we really
want, in the future, to support interval queries, we can do that using IxSet and
an appropriate index on our stored date.

## What I shipped
At the end of Sunday, I finally had a prototype which is working nicely; I didn't
feel bold enough to demo the whole thing (and I regret it, I should have probably
done it!). This is the architecture in a nutshell:

* Whatever a user hit an endpoint, snaplet-perf will compute the start and the
  end time of this request, the endpoint hit and the HTTP method, creating a
  domain object called `Measurement`.
  
* Such a measure will be written into a `TBChan` and processed asynchronously
  by a worker thread.

* The worker thread will store the new measure into acid-state, which will
  keep into memory a `DataStorage` data structure which is just a `Map` in
  disguise, going from `EndpointHit -> [Measurement]`.

* Once the measurement is stored, the worked thread will synchronize with
  a websocket worker using an `MVar`.

* The websocket worked will send out the measurement, encoded as JSON, to
  the client.

* The client will use such JSON to update (or create) a nice graph (made
  with a library called [rickshaw]() to display the measurement).

A lot of this code was already there, I specifically did the worker overhaul
(to use acid-state), the websocket part and the UI bit. The result can be seen
in form on a small screencast here:

url

Note how is possible to monitor the new incoming traffic without refreshing the
page!

## Drawbacks
The drawbacks of the current implementation:

* It's using an endpoint to retrieve ALL the stored data to populate the graphs
  before listening for new measurements. This will make `/perf/dashboard` slower
  and slower and the samples data grows. We might simply allow a configuration
  parameter to limit the data retrieved upfront, or using some sort of async
  call to gradually populate the data in chunks (here is where a streaming library
  wins!)

* Due to my breaking change (aka removing zoom-cache) I can't simply merge this
  into master without having Doug approve my design choices. The project is
  hidden, so it would be mean from me if I just go in, fork my own fork and release
  this alternative version as public, as it contain work of other people as well.


## Conclusions
As every hacking project, is rough around the edges and can be polished further.
I don't know what the future of this project will be, as its diverging from the
"official one", but I hope Doug seeing this video will accept my beefy PR :)

Alfredo
