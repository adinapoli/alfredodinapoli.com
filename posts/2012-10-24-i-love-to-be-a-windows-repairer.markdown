---
title: I love to be a windows repairer
description: Sometimes fixing is better than creating
tags: self, programming
---

##I love to be a windows repairer

This is something I was thinking about yesterday night, trying to get asleep.
Some of my colleagues are very argumentative about the role a computer
scientist must have: someone thinks that only who builds abstractions (call 
him architect or whatever) is the only person worth respect in a company.
Others, like me, thinks that any programming role could be something
extremely interesting and compelling if you see it on a different perspective.

### The windows repairer

I don't know if someone come up with this definition, but I could easily
define myself a "windows repairer" or a "windows fixer", if you prefer. The
"broken window" paradigm was popularized by Andy Hunt and Dave Thomas in
their masterpiece
[The pragmatic programmer](http://pragprog.com/book/tpp/the-pragmatic-programmer).
In one of the first tip of the book they introduce the _broken window principle_:
sometimes, all you need to make your software rot is a broken window, intended
as a single messy point where you introduce an hack, code duplication or a
not-optimal design choice. The _"broken window"_ is dangerous because it rapidly
becomes a code quality metric. You start saying "Yea, after all I already
used that hack there, so I'm gonna use it too..", and pretty soon you got
a building with _two_ broken windows, and the overall code quality
degrades.  Conversely, if you have a clean code, you are more reticent about
introducing and hack or a poor programming choice. You _feel_ the quality and
you are proud of the "perfect temple" you have created.

### The real world

Most of the time, though, you can't program that way. The real world is full of
broken windows. Reasons are multiple: short deadlines, throw away code,
inaccuracy and so on so forth. You sometimes are put on a legacy project and
they say to you "We have this huge amount of spaghetti code, and we want
brand new features on top of that". Here programmers are split in a half:
One half just hate to maintain and improve legacy code, other half just love
it. I don't know if I really "love" to maintain spaghetti code, but I think
that sometime fix is better than create.

### The false misconception

There is this false misconception that brings programmer to prefer
evergreen project. Sure, you must be creative to design a new architecture,
but I think that the same level of creativity can be achieved modifying legacy
code. How? Let's take as motto what uncle Bob says in his authoritative book
_Clean Code_: he says that **we should always leave the camp cleaner than 
when we have found that**. What does it mean? That we should put all our
effort in improving the quality of the code we write and maintain.

### Work as a surgeon

A surgeon operates on a patient, sometimes working and reversing a very
bad situation. He _cures_ the patient, not just throw away his work. He
cares in what he does, because he knows that a human life is at stake. As
a surgeon, we must care about our craft, even if craft is something
we have inherited. We can start developing adequate code coverage to be confident we
won't break anything already coded, and then we can start operating. We can
begin simplifying messy functions, extracting other functions/methods and
testing everything as we proceed. It can become fun and addictive: how we
can further simplify this code? What lovely one-liner can we use? If we
love functional programming, we can replace our messy ```for``` with ```map```
or using ```filter``` elegantly to restrict our result. If we operate in the
Java World, for example, we can use the _Guava_ libraries to prevent us from
the dreadful ```NullPointerException```. Conversely, if we just think
"this code sucks, I'm not gonna to maintain it, I'm gonna write my layer
on top of that" sooner or later you we'll be caught up into the "broken
window paranoia". Since the code quality is low and most of your design
choices were biased by that crappy code, you'll start to see your code with
the same nasty glance you look at the legacy code. You don't feel this 
system like _your craft_, so you are not too motivated in make that
beautiful.
Conversely, if you spent a certain amount of time improving the existing
code base, not only you'll end up with a more robust system (which is
always an advantage) but you'll begin feeling proud of what you have done:
"Look at this code", you'll say - "it was crap three months ago and now is
fully of best practices!". You will begin considering that code _a your craft_,
and you'll be more cautious when you'll modify it. You will be more
motivated in keeping as clean and as beautiful as you can.

### Conclusion
Sometimes work with existing code sucks, but you have to put up with it.
You have two ways to approach the problem: the "wrong" way, when you want
to hide the legacy code and the "right" (at least for me) way to do that.
_Embrace the existing code_, turn something ugly into something beautiful.
Be a savvy artisan and like a sculptor painstakingly improve your creation.

In other terms, **fix your windows**.
