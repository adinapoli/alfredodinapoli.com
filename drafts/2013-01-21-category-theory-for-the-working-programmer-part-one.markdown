---
title: Category Theory for the Working Programmer
description:
tags: haskell, programming, fp
---

##Category Theory for the Working Programmer
This is an experiment, and it's a part of an ongoing series. I've decided
that after a billion of introductory posts about Haskell, it would have been
cool to talk about something more deeply involved inside the mathematical core
of the language. Accordingly with this, I was also keen on learning more about
category theory and since the best way of learning something is trying to
explain it to others, here I am. So category theory huh? In a programming
post? Is this madness? At a first glance, it might seems so. During this
series I won't be too formal as regards the mathematical side of my 
dissertation, and this may make some eyebrow to rise, but my objective is not
turning these posts inside a rigorous dissertation about CT, but better give
the gist of what is about, without delving too much inside the math part.

###What is Category Theory in practice?
Is the mathematical study of abstract algebras of functions, so essentially
how they relate to each other and how they compose. Why this matters at all
in programming? Well, most functional programming languages (e.g. Haskell 
above all) draws from CT a lot of their ideas and even design! As as stated
in the excellent Awodey's book "Category Theory" (which I'm following closely,
otherwise there won't be any chance for me to talk about this stuff!), this
matters because ```functions are everywhere!```.
Think about it! This holds even more in Haskell where pretty much everything
is a function.

##Is Category Theory worth learning?
The answer is [controversial](http://www.reddit.com/r/haskell/comments/16i322/whats_the_importance_of_category_theory_in/).
After all, there are a lot of very good Haskell programmers out there which
don't know anything (or very few) about CT, but I think that this kind of
knowledge can be very beneficial to think "out of the box", as well as
to sharpen our analytical side of the brain. Ok but, where to start? CT is
a very *ABSTRACT* topic, so I think it will be very difficult trying to explain
it in simple term, but still retaining that "real world" outlook which makes
this post worth its name. I thought that the best idea would be to
intersperse theoretical stuff with concrete example in Haskell. Let's see if
I can maintain my promise!

##A formal definition
As stated before, CT is the study of abstract algebras of functions. To be more
precise, CT stems from the idea of a system of functions among some objects:
