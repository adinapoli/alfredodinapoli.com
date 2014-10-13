---
title: Fun with Dependent Types in Haskell
description: A perhaps-not-so-interesting example
tags: haskell, type-families
---

Inspired by recent [Oliver Charles' talk](https://skillsmatter.com/skillscasts/5356-oliver-charles) at the
Haskell Xchange (which was a blast, btw), I felt
compelled in trying to fiddle around with type families
and dependent types in Haskell to fix the concepts in my
long term memory. Even though those weren't new to me, and
Ollie's talk was very accessible and nicely delivered,
I wanted to brainstorm a bit to see what I could get
out of it. First of all I read the [suggested paper](http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf), which was a really
nice refresher on the topic (and much more!). **This is a literate Haskell
post. Feel free to load it inside GHCi**. First a bit of
throat-clearing:

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> module Main where

In this post we'll index our datatypes by lifted naturals instead
of lifted Peano number, so we leverage the handy `GHC.TypeLits`
module:

> import GHC.TypeLits

Let's first start with something simple: In case you need a quick
reminder, a `type family` is essentially a function _at the type
level_. Let's write the `SillyId` type family: it will take a 
type and will return the type itself:

> type family SillyID k1 :: *

Here we are using an _open type family_, as we can see from
the following instances:

> type instance SillyID Bool = Bool
> type instance SillyID Int = Int

In a nutshell we are mapping these two types to themselves. Think
as a sort of `id`, but at the type level. We can "use" a type family
in a function signature to transform types:

> sillyBool :: Bool -> SillyID Bool
> sillyBool True = False
> sillyBool False = True

So far this should be very basic and boring stuff. Let's move over.

Type safe byte chunks
=====================

To motivate this blog post, let's imagine your boss asked you to
write a program to handle chunks of raw bytes. As every good 
Haskeller you like types and would like to write your program
in a way that the invariant are specified in the types.
Your first thought is to model a chunk as a vector, and almost
unconsciously you write something like this:

> data Chunk :: Nat -> * -> * where
>   CNil :: Chunk 0 a
>   (:*:) :: a -> Chunk n a -> Chunk (n + 1) a
>
> infixr 5 :*:

The `TypeLits` module we imported promotes every natural into a
type, allowing us to write this extremely nice vector. It also
promotes some operations for naturals as well, so that we can
simply express the increment in size of our `:*:` operator as
`Chunk (n + 1) a`. This doesn't give us much more than standard
vectors though: we could still write, for example, the following:

> unsafeHead :: Chunk n a -> a
> unsafeHead CNil = error "Ops!"
> unsafeHead (x :*: _) = x

Uhm, this is not Haskell-y! Let's fix this first. What we need
is a way to force the compiler to reject a function like this which
is applied to an empty `Chunk`. There are tons of ways to solve this,
so we'll pick just one solution for didactic purposes. Let's first
write another type family, but these time a _closed_ one: this has
the nice advantage to give us the control in a way that we cannot
add more cases externally (as orphans):

``` haskell
type family NonZero (n :: Nat) :: Bool where
  NonZero x = _
```

What we can write inside the _hole_? This will certainly work:

``` haskell
type family NonZero (n :: Nat) :: Bool where
  NonZero 0 = False
  NonZero x = True
```

If you are not familiar with type families this is a type-level
function which is expecting a `Nat` and will return a `Bool`: what
we see there, though, are not types but **kinds** (remember: everything
is lifted one level up). This is why we return `True` or `False`, which
are two promoted values into the type level (this is what the `DataKinds`
extension is for). If you want to visually enforce the value/type separation,
you can prepend each promoted datatype with a `'`, which can be omitted
when there is no ambiguity. To write the version of `NonZero` we'll use,
let's start from the intuition: we would like to _compare_ the input
`Nat` with `0`, to assess whether they are equal or different (being
naturals we do not have to worry about negative numbers). It turns out
that `GHC.TypeLits` already have exactly what we need:

``` haskell
type family CmpSymbol m n :: Ordering
```

Unfortunately this yields an `Ordering`, but what we want is a
`Bool`, so we need a type level function from `Ordering -> Bool`.
Let's try this:

> type family EqualTo (n :: Ordering) (m :: Ordering) :: Bool where
>   EqualTo GT GT = True
>   EqualTo LT LT = True
>   EqualTo EQ EQ = True
>   EqualTo x y = False

Nice. Armed with this, we can write our `NonZero` type family:

> type family NonZero (n :: Nat) :: Bool where
>   NonZero x = EqualTo (CmpNat 0 x) LT

And finally write a `safeHead`:

> safeHead :: NonZero n ~ True => Chunk n Int -> Int
> safeHead (x :*: _) = x

The novelty here is just that we used type level equality to 
basically say: "Please GHC, I want this function to be called
only on types where the constrain `NonZero ~ True` holds. You
can think of `~` as an equal sign. To give you the intuition
the type level function needs to yield `True` at compile type for
this function to typecheck.

This will compile:

> safe1 :: Chunk 1 Int -> Int
> safe1 = safeHead

But this won't:

``` haskell
unsafe1 :: Chunk 0 Int -> Int
unsafe1 = safeHead
```

Ok, this was a lot of work for a simple total function, but it
paved the way for more interesting stuff! 

More interesting stuff
======================

Your boss looks at your code and says "Ok, but this stuff can be
done in Idris or Agda in few lines of code. Not impressed.", so you
go back to your desk and keep coding. The new requirement is to
write a function which operates on byte chunks. So we want to 
write a function where only chunks which are _multiple of 8_ are
allowed to typecheck. In a non-dependently typed world, a Java
programmer would write:

``` java
public static MultipleOf8 {
  public Int multOf8(Chunk<Integer> chunk) {
    if !(multOf(8, chunk.length())
      throw NotMultipleOf8Exception(chunk)
    // do stuff
    return chunk.head();
  }
}
```

As Haskellers, we might try to do better than the imperative version
and wrap the possibly-failing computation in a monad:

> -- Positive multiple of, classic Haskell inductive function
> posMultOf :: Int -> Int -> Bool
> posMultOf _ 0 = True
> posMultOf x y = if y < 0 then False else posMultOf x (y - x)

> chunkLen :: Chunk n a -> Int
> chunkLen CNil = 0
> chunkLen (_ :*: xs) = 1 + chunkLen xs

> multOf8Mb :: Chunk n Int -> Maybe Int
> multOf8Mb CNil = Nothing
> multOf8Mb c@(x :*: _) = case posMultOf 8 (chunkLen c) of
>   True -> Just x
>   False -> Nothing

We could certainly use `NonZero` to avoid the `CNil` equation,
but this is not very satisfying anyway: wouldn't be nice to enforce the
invariant of "must be multiple of 8" into the type system?
What we really want is a "type level posMultOf" function, something which
will yield a type-level `Bool`. Let's look again at the definition:

``` haskell
posMultOf :: Int -> Int -> Bool
posMultOf _ 0 = True
posMultOf x y = if y < 0 then False else posMultOf x (y - x)
```

First equation is easy. Second equation has a couple of pain points:
* We need to do arithmetic (i.e. `y - x`)
* We need to do conditional branching
* We need comparison (i.e. y < 0)

Luckily we can address the first point thanks, once again, to `TypeLits`,
which gives us, out of the box, exactly this. But now we need a
_type level if function_. Can we write it? It is surprising easy to do so!

> type family If (pred :: Bool) (thenB :: Bool) (elseB :: Bool) :: Bool where
>   If True t r = t
>   If False t r = r

Nice! What about the last one? We need `y < 0` but all we can do at the
moment is say `y is non negative`, but here we need `y is negative`.
Did you guess already? We need a type-level `not` function!

> type family Not (b :: Bool) :: Bool where
>   Not 'True = 'False
>   Not 'False = 'True

And finally we can express `PosMultOf`:

> type family PosMultOf (n :: Nat) (m :: Nat) :: Bool where
>   PosMultOf x 0 = True
>   PosMultOf x y = If (Not (NonZero y)) False (PosMultOf x (y - x))

Which I find remarkable: Look at how similar is to the value-level
one!

``` haskell
posMultOf :: Int -> Int -> Bool
posMultOf _ 0 = True
posMultOf x y = if y < 0 then False else posMultOf x (y - x)
```

Armed with this, we can now write:

> multOf8 :: (NonZero n ~ True, PosMultOf 8 n ~ True) => Chunk n Int -> Int
> multOf8 (x :*: _) = x

> correct :: Chunk 8 Int -> Int
> correct = multOf8

> correct'' :: Chunk 16 Int -> Int
> correct'' = multOf8

As expected, these won't typecheck:

``` haskell
emptyChunk :: Chunk 0 Int -> Int
emptyChunk = multOf8

bogus :: Chunk 7 Int -> Int
bogus = multOf8
```

Nice!

Conclusions
===========

Harnessing dependently types techniques in Haskell is
easy (not as easy as Idris or Agda though), fun, and yield
code with stronger safety guarantees.
Mastering type families takes a while, but is definitely worthwhile!

Acknowledgements
================

* Oliver Charles for the nice talk
* Richard A. Eisenberg and Stephanie Weirich for the nice paper
* You, the reader, for reading it till the end.
