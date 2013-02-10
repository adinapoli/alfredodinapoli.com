---
title: Demand more from your programming language
description: Simple but often forgotten thoughts on Haskell
tags: fp, haskell, scala
---

## Demand more from your programming language

First of all, this is not a post about "Your language is better than yours".
No. So no flame war incoming. This is simply a post about some simple concept
in Haskell that sometimes we (at least I) forgot because I give them away as
"granted", but that can be extremely useful for the newcomer. This post is
shameless rumination of part of the excellent talk Doug Beardsley gave at
NYC Haskell meetup.

### We live in an imperfect world

As the great programmer he was, Edsger Dijkstra once said _"If debugging is
the process of removing software bugs, then programming must be the process of
putting them in"_. This may sound hilarious at first, but I believe is
incredibly true! We are humans, and we will always made mistakes; because we
are not focused enough, because we are tired or bored for a particular task,
because we are distracted by a funny joke of a colleague in the middle of a
coding session, or "put your favourite reason here". The point is, that is
difficult sometimes to get things right, so we want the machines (namely our
computer!) do double check for us, getting rid of some obnoxious bug we could
be not aware of or just missed in our programming frenzy.

### The dynamic languages

Dynamic languages have tons of excellent features (they are pragmatic, allow
rapid prototyping etc etc) and I still love hacking with Python with a have
a simple script to code or I want to do something programmatically (e.g.
parse a text file, manipulate it, show some stuff, rinse and repeat), but they
most of the times lacks a sophisticated type system to help us catch bugs.
Granted, they *do* have sophisticated mechanism under the hood to do type
inference, but this is not the point I want to make. Take this excerpt of 
python code, I had in production for a while:

``` python
class Transaction:
  def __init__(self, id):
   self._id = id

  [...]

def warn_user_about_unprocessed_transaction(transaction):
  send_email(transaction.id)
```

Now, what happened is that, unsurprisingly enough, this code is risky. I won't
insult your intelligence with another dissertation about type safety, but too
often I ended up doing something stupid like this:

``` python
[...]
transaction = myobj.id
warn_user_about_unprocessed_transaction(transaction)
```

Granted, this is stupid code, but when you fiddle with an ORM and IDs is
easy to confuse the object id from its whole representation. The scariest 
thing is that this thing will fail only when I will hit that code for the
first time! Again, there are some excellent static analysis tools over there,
but they sometimes does not work too well. The point here is that we are kinda
walking on a mine field; we don't have type signature (Python3 makes them
optional, which is a good thing!) and worst of all we tend to not trust our
code anymore, because we have to put a lot of extra effort in ensuring we
are passing the correct type to every function!


### Strongly typed languages at rescue

Now, suppose we were writing the same in Java/C/C++ or whatever programming
strongly typed programming language. Here the compiler gives us an invaluable
help in catching stupids bugs like the one just shown. The classic objection
(which I subscribe!) is that they are far too verbose and the type system 
sometimes really clutters your code (think about Java's checked exception
and your monolithic function signatures!). But whether hardware and technology
evolves, the *way* we build software seems to have reached a plateau. How come
we are using the same technology and imperative techniques of more than 30
years ago? Can we do better? Well, we functional programming zealots think we
can.

### Functional programming and purity
Sometimes we forget that functional programming focuses on two different
aspects:

* Purity

* Type safety

While the second is something we can achieve also with the imperative languages,
types are a totally different kettle of fish in the FP land. The type system
is **a tool** to explore the design space of a new piece of software. When we
replace interfaces with functions, we see a sort of "pipe system" emerging,
when we focus not on **how** do something (think about recipe, do step A,
then step B, etc) but **which transformations** are required to produce the
desired result. By "pipe system" here I mean a system composed by pipes and
junctions which connect two pipes together. Our pipe is a function, the junction
is the type signature. We can only attach a pipe to another one only if the
junction shape matches. If we weren't doing so, our pipe would leak! Is not
difficult to see where this analogy bring us. The *leaking* is the concept
of state! Being *sure* that our pipe won't do anything "stupid" with our
input data make us more confident about the outcome, while the type system
take care of us that, even before putting the water inside the pipe (namely
the "runtime computation flow"). Is like if a plumber would come and check
for us the entire pipework, saying "Ah! Here the is a leak, you are using
two pipes with different junctions!". If our plumber says "Ok, it's safe to
go", we can open the tap and let the water go through our system, without
worrying that the pipework will break suddenly, making the entire water go
lost. Now, bear in mind one fundamental thing: I'm not saying anything about
the result, I'm not telling you that acting this way will save us from all
the bugs, this is simply impossible, I'm just saying that this will save you
from having a leaking pipe, that's all!

Now we compared the type system and the compiler to a plumber which ensure
our pipe don't leak; but pipes can leaks for different reasons! Imagine that
we have our pipe perfectly sealed, with all the junctions perfectly aligned.
If you think about this, this is something we could achieve also in an
imperative language and, being provocative, also in Scala! After all, Scala
has an excellent type system, so we would expect that program in Scala should
never leak, right? Well, unfortunately is not the case. Scala programs *may
leak* but not of a "junction" leaking. Here's where purity comes into play.

## Being pure with Haskell

Now, imagine this scenario. Your pipework, along the way, has something 
unexpected. The pipes goes inside another premise we don't
have the control over our pipework anymore. While inside the premise, another
plumber decide to modify our pipe, insert a special water cleaner and then
"re-inject" the water inside the old pipe system. Wait, we have side effect!
How is that possible? We have type safety, the world should be a better place
to live, shouldn't it? Well, type safety helps, but purity is the big winner
here. Being pure means **clearly separates** what is pure from what is not,
or, to say it in another way, separate **the context** from the data. Not
going inside the details, because this is something I don't to turn this
stream of thoughts in a tutorial, but this is pretty much the essence of
**monads**. Monads are like a special "case" we put around our pipes: have
you seen electrical cables? They have different colors based on what their
function is; a ground cable will have a different color from the main one.
That's it: electrical cable have different color to put them in **different
context**. We will do the same with our pipes; we'll wrap them with a special
case, every case with a special color and with special rules; "naked" pipes
cannot be touched for their entire lifetime. This means that the "plumber
worldwide association" agrees that they will never touch a naked pipe: the
only plumber entitled to do it is the one who originally build the pipework.
Then they could agree that pipes with a red case can be modified but with
a special constrain; the can *never* lose the original case and color. In
a nutshell, once we wrap our pipe inside a red case, we **cannot** turn them
into a green pipe or a naked one, we can't lose the information about the
pipe being a red one!

## How the hell this is relevant?

If you keep up with my example, you have pretty much understood what a monad
is about. So let's turn this inside sexy Haskell code and you'll have your
"a-ha!" moment, I promise. Let's start with this code:

``` haskell

square :: Double -> Double
square x = x * x
```

Ladies and gentlemen, the **naked** pipe. See the clutter-free signature? We
are saying that we have no context around us, this function is **pure**.
We are **guaranteed** that this function will **never** talk to the outside
world. It's just pure, plain water flowing inside our pipe. Now let me show
you this overly-verbose example, just for teaching purposes:

``` haskell

getName :: IO String
getName = getLine

main :: IO ()
main = do
    name <- getName
    print $ "Hello " ++ name
```

This get your name from the stdin and greets you. Oh! See that fancy ```IO String```?
Is the red pipe! Without getting too deep inside the details (tons of tutorials out
there explaining all you need to do about monads) we put a special "red case",
called ```IO``` around our naked pipe. Now we know, simply looking at this
function that ```getName``` is a function of a special breed, it can talk
to the outside world! It could talk to the DB, getting involved in weird
thread contentions or fire up a missile to destroy the world. But the point is
that, just like the "plumber worldwide association" agreed, you can't remove
this "case" from your function. It's just like a sort of masonic motto:

<div markdown="1" class="glance-box">
"What happens in the IO monad, stays in the IO monad!"
</div><br>

In other terms, we can't "escape" from the IO monad, now to use this function
we have only two choices:

* Use this in isolation, but we can't coerce an ```IO String``` back to a ```String```

* Use this only together with other red pipes! (like we did calling it from ```main```)

See the wonderful pattern which is emerging? Now we have safety a type level,
namely our junctions, but context-safety thanks to different "case"! So now
we have a nice pipe, with all the junctions perfectly fitting and colored of
a shine red all way down. Much better.

Compare this with, for example, Scala:

``` scala
def square(n1: Double, n2: Double): Double = {
  mutateTheWorld();
  sendMissileToMars();
  n1 * n2
}
```

In Scala, and in every other programming languages without monads (and its
derivatives) we can only ensure our pipe won't leak, not that our pipe won't
be modified.

That's **one** of the power of monads. This is one of the power of Haskell.


