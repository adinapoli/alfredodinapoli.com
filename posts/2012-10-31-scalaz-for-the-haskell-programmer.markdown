---
title: Scalaz for the Haskell programmer - Part 1
description: A (hope) useful vademecuum about scalaz programming
tags: fp, scala, haskell
---

[scalaz](https://github.com/scalaz/scalaz) is an awesome library that extends
the Scala Core, providing FP goodies that every Haskell programmer loves and
needs. Yes, I'm obviously talking about ```Functors```, ```Monads``` and
other strange beasts from the Category Theory panorama. There is a lot of
learning material for scalaz, but I've basically discovered that is not easy
to organize your thoughts if you are an Haskell programmer, so I'm basically just
writing this primarily as a mental note, but I hope it will be useful for
others too. The tutorial will be focused on **scalaz 7**, so be wary if you
are not planning to switching any time soon.
Before we start, I've found this little gem that compares, side by side,
a lot of features from different programming languages:

<div align="center" markdown="1">
[Hyperpolyglot cheatsheet](http://hyperpolyglot.org/ml)
</div>

### Setting up an sbt project
If you are lazy and want to start right away, I suggest you use my 
[giter8 template](https://github.com/adinapoli/scalaz-revolver.g8)
to have a plain project with scalaz and
[sbt-revolver](https://github.com/spray/sbt-revolver). The template also
imports for you scalaz in the sbt console, so you can start playing right
away.

### Monads and Control.Monad functions
Most tutorials starts with ```Typeclasses```, data structures declaration or other
important aspects. Here, I want to start from Haskell's main draw, ```Monads```.
This should be familiar:

```Scala
some(3) >>= (x => some(x+1)) //yields some(4)
```

Unsurprisingly, it returns ```some(4)```. It this code is not straightforward
to you, you won't find the monad part covedered. This is only a sort
of "quick mental mapping" for the Haskell programmer that wants to program
in a more functional oriented way in Scala.
This code should be familiar too:

```Scala
some(3) >> some(4) //yields some(4)
```

Remember how ```(>>)``` is defined in Haskell:

```Haskell
(>>) :: (Monad m) => m a -> m b
```

#### replicateM

```Haskell
replicateM :: (Monad m) => Int -> m a -> m [a]
```

Has its equivalent in:

```Scala
some(2) replicateM 3 // yields some(List(2,2,2))
```

As you can notice the syntax is not as terse an in Haskell, due to the nature
of the two languages; in Scala, everything is an object, and Haskell
behaviors are simulated using ```traits```. This obviously force the
programmer to write code that resemble method invocations. When allowed, I
always prefer the "dot-free" syntax (like the example above), because give
the code a more functional look.

#### filterM

```Haskell
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
```

```Scala
List(1,2,3) filterM { x => some(x % 2 == 0)} //yields some(List(2))
```

It would have been nice to have the Haskell's equivalent of ```return```,
but I wasn't able to find nothing like that in scalaz. If you are aware of
something different, please let me know, I will be happy to update this post.

### Functors
Another important tool in the functional programmer belt are functors. They
are supported in scalaz too (it's obvious, because ```Monads``` are ```Functors```,
so it scalaz supports ```Monads```, why not ```Functors```?). scalaz's
developers have decided to polish a bit the syntax, so you have to call ```map```
instead of ```fmap```. You may or may not like this choice:

```Haskell
fmap :: (Functor f) => (a -> b) -> f a -> f b
```

```Scala
some(2) map (x => x * x) //yields some(4)
```

### Applicative
In layman's terms, we can call an ```Applicative``` a ```Functor``` on steroid.
In Haskell, it's very common to use them to aggregate results of a 
non-deterministic computation. Take a look to this piece of Haskell code, that
can be quite mind-blowing for the apprentice functional programmer:

```Haskell
(+) <$> [1,2,3] <*> [4,5,6]
```

This yields all the possible combination of summing the values from the to
lists together. In this case we obtain the list ```[5,6,7,6,7,8,7,8,9]```.
In Scala, we can obtain the same result with this syntax:

```Scala
(List(1,2,3) |@| List(4,5,6)) { _ + _ }
```

Where ```|@|``` yields an ```ApplicativeBuilder``` (I'm not enough proficient
with scalaz to speculate about this design choice).

### Monoid
In Haskell, the two most important function every ```Monoid``` has are
```mempty``` and ```mappend```. In scalaz you have ```mzero``` and ```|+|```
instead:

```Haskell
mempty :: (Monoid a) => a
mappend :: (Monoid a) => a -> a -> a
```

```Haskell
mempty :: [Int] --returns an empty list
[1,2] `mappend` [3,4] -- [1,2,3,4]
```

This is roughly the equivalent in Scala:

```Scala
mzero[List[Int]]
List(1,2) |+| List(3,4)
```

Well, I have to say that amoung all the scalaz stuff, the ```Monoid``` part
is what I like less. Why? Because it doesn't stick with the original Haskell
and matchematical definition. Wolfram MathWorld sais that:

<div class="glance-box">
A monoid is a set that is closed under an associative binary operation
and has an identity element[...]
</div><br>

In layman's terms, every ```Monoid``` is defined respect to an operation.
This explain why an ```Int``` can't be a ```Monoid``` per se: which operation
should we consider? ```+``` or ```*```? You can prove this trying to ask
Haskell for the "zero value" of an ```Int```:

```Haskell
mempty :: Int

No instance for (Monoid Int) ...
```

And the same applies for ```mappend```: how can Haskell know how to append
two ```Int```?

```Haskell
3 `mappend` 4
```

To solve this, we wrap our ```Int```(s) in types that are instances of
```Monoid```, for example ```Sum``` and ```Product```:

```Haskell
mempty :: Sum Int
Sum {getSum = 0}

mempty :: Product Int
Product {getProduct = 1}
```

Now, with this formalism, we can ```mappend``` our ```Monoid```, because
now we have an operation that characterize the ```Monoid```:

```Haskell
Sum 4 `mappend` Sum 5
Sum {getSum = 9}

Product 4 `mappend` Product 4
Product {getProduct = 16}
```

Conversely, Scala is not so strict, and allows us to do this:

```Scala
9 |+| 15
```

Which I don't like very much. (To the expert reader? What am I missing? Maybe
is ```|+|``` an alias for a ```Monoid``` closed on ```+```?)

### Pipelines and function composition
This actually isn't an Haskell features, but for example F# has the nice
pipeline operator ```(|>)``` to give the code a dataflow structure. You can
use the pipeline operator to implement function composition as well:

```Haskell
floor . sqrt $ 10 --equivalent to floor(sqrt(10))
```

```Scala
10.0 |> sqrt |> floor //yields 3.0
```

As you can see, the computation "flows" into two different ways: in the
former example from right to left, in the latter from left to right, but the
result is the same. A nice that we got for free here is a bit more of type
safety: whilst Haskell accept the above code, Scala refuses to compile due
to a type mismatch, if we pass ```10``` to the pipeline operator. Nice.

## Conclusions
This first part was dense, but also useful to get myself acquainted with
scalaz. There is still a lot to cover (for example ```Comonad```, ```Arrow```
and more), so there is room for a part 2. I think scalaz has a lot of
potentials, and I'll be happy to use in a project if I have the chance. I
believe it helps to get rid of the "OO-flavor" Scala inherited from its
Java vestiges. Before the farewell, take a look at this two blog post, that
I've read to gather part of the material from ```Applicative``` and 
```Monoid```:

* [A Small Example of Applicative Functors with Scalaz](http://www.casualmiracles.com/2012/01/16/a-small-example-of-applicative-functors-with-scalaz/)
* [Having Fun with Monoid in Scalaz Seven](http://voidmainargs.blogspot.it/2012/02/having-fun-with-monoid-in-scalaz-seven.html)

Happy hacking!
