---
title: Disengage and live better
description: A small trick to disengage
tags: self, programming
---

##Disengage and live better
I'm reading a book called [The power of full engagement](http://www.amazon.com/Power-Full-Engagement-Managing-Performance/dp/0743226755),
which basically states that is important to alternate periods of great focus
(engagement) with period of relax (disengaging). This theory is supported from
several concrete example involving tennis athletes or professional, where the
best results where obtained building up stress (pushing ourself out the "comfort" zone)
and then refilling our energy reserves simply "disconnecting" from the work or
from whatever causes us stress.

As a developer, I experience the same constant sense of duty which may be
familiar to any other professional: sometimes is very hard to have your mind
free of thoughts and is not uncommon that I find myself dwelling on work stuff
also in the evening. So I'm trying, as I progress in assimilating all the principles
of the book, to disengage in the evening, simply doing something I like (watching 
Masterchef, programming, reading blogs, learning stuff, etc). But I've encountered
and extra complication I've promptly addressed.

### The "blue dot" mobing
Do you recognize this?
<div align="center" markdown="1">
  <img src="/img/2013/blue_dot.png"/>
</div>
This could be the nightmare of every programmer! I invented the term "blue dot mobing"
to describe a high status of stress deriving to constantly see the dot turning
from gray to blue. Where I work, we use Github massively to develop all our
software. This often implies that our customers have read access to our
repository: this is great because we can provide them with constant feedback,
but the other side of the coins is that often they fire up issues/question/bugs
at every time during the day, hampering my effort to disengage. Why?
Well, I enjoy learning new technology or fiddling with OSS in my spare time,
and the last thing I want to see in the Github homepage is the news feed
remembering me that Monday morning I will have to deal with some project issues!
The problem for me lies in the fact that I subconsciously go to ```https://github.com```
and that I usually refresh the page clicking on the small Github logo in the
top-left corner of the page. What does this imply? Well, that I see the news
feed again, and again, and again! I've tried to filter news feed (also with
browser's plugins) but every solution was quite limited. What to do then?

### URL redirecting to the rescue
The solution is as simple as useful. Since Github uses ```https``` by default, we
can't simply hardcode the redirect inside ```etc/hosts```. I've discovered this
useful plugin googling around: is called [Redirector](https://chrome.google.com/webstore/detail/redirector/lacckjdlmkdhcacjdodpjokfobckjclh)
and it can be daunting if you don't play around with it a bit. The idea is
to be redirected to a new URL everytime we hit ```https://github.com```: yes,
but which URL? For me, the solution was to point to my [dashboard](https://github.com/adinapoli):
this way, I can still see the blue dot (but I can mark all notification as read and solve
the problem), but no more news feed bringing me back to my duties!
Configuring Redirector can be tricky, but this revealed to work:

```Shell
Match (RegExp): ^https://github\.com/$
Substitution (RegExp): .*
Replacement: https://github.com/adinapoli/
```

Done! Now (remember to enable Redirector clicking to the appropriate button,
you'll see a blue icon in the far left corner of the URL bar) everytime we'll
hit Github we'll be forwarded to the dashboard.
Now I have my peace of mind.

Happy disengaging,
<br>
Alfredo

