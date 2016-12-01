---
title: "iconv-typed: An experiment in API design and type safety"
tags: haskell, fp
---

```
Summary: I'm releasing a type safe version of the iconv library, discussing my
API design choices and asking for feedback from the community.
```

<hr/>

I'm slowly making progress in an Haskell [piece table](https://github.com/adinapoli/piece-table) library
which could be used as a high performant data structure for text manipulation. The typical use case
there would be writing a text editor in Haskell, something I had in the back of my mind doing (for fun)
for a while.

So far the assumption I have made whilst developing it is that user text would be encoded/decoded as `UTF-8`, but in
the real world, though, this is simply not true! That's where [encoding](http://kunststube.net/encoding/) comes into play.
I won't get into too much detail about the `piece table` library (is not that interesting in its current shape!), but this
should set the scene on why I needed text encoding in the first place.

In Haskell we have a couple of choices when dealing with text encoding: we can use some [functions](http://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text-Encoding.html) provided
directly by the `text` library, use the [encoding](http://hackage.haskell.org/package/encoding) library or use
Duncan Coutt's [iconv](http://hackage.haskell.org/package/iconv-0.4.1.3/docs/Codec-Text-IConv.html) library. I really like
`iconv` because it has such a simple API and it doesn't assume anything on the input: the latter is given as a "blob of binary data"
and it's up to me to decide how to interpret it.

Despite its simplicity, I always thought the library also had great potential for things to go wrong: first of all, an `EncodingName` is
simply a `String`, which the programmer can mispell and spend hours debugging why is program in producing garbage. Secondly, it requires
the manual step of retrieving the list of available encodings from the system, typically piggybacking on the underlying C/GNU library.
This is why today I'm releasing [iconv-typed](https://github.com/adinapoli/iconv-typed) mainly to gather feedback from the community.
It's such a simply abstraction over `iconv` I'm surprised nobody thought about something similar, but maybe that's because it's so
simple people have wrote it in their own projects without releasing it, or simply because maybe it has shortcomings I haven't anticipated!

## A taste of the API

APIwise, the library should feel familiar with the original `iconv`. Compare this short example using the `iconv` library:

``` haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Text.IConv

main :: IO ()
main = print $ convert "UTF-8" "LATIN1" "hello"
```

With the equivalent in `iconv-typed`:

``` haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Text.IConv.Typed

main :: IO ()
main = print $ convert @"UTF-8" @"LATIN1" "hello"
```

As you can see it's almost identical except for the fact we are using `TypeApplication`'s `@` operator.
If we mispelled by accident `UTF-8`, we would get a type error. Profit! But how does it work?

## Type families to the rescue!

Conceptually, it's very simple: it fetches all the available encodings in a platform-dependent way (mainly invoking `iconv -l`
under the hood), and then generates a closed [type family](https://wiki.haskell.org/GHC/Type_families)
via [template Haskell](https://wiki.haskell.org/Template_Haskell) to basically constrain the `Symbol` universe only to ones
matching a valid encoding. A code snippet will demostrate this much better! We first commit the biggest sin in the whole
Haskell universe and we get the encodings via `unsafePerformIO` [1] (it requires the `Shelly` library):

``` haskell
getAvailableEncodings :: [EncodingName]
getAvailableEncodings = unsafePerformIO $ shelly $ silently $ escaping False $ do
  map T.unpack . mconcat . map T.words . T.lines . T.strip <$> run "iconv" ["-l"]
{-# NOINLINE getAvailableEncodings #-}
```

... and then we generate the type family where each instance would look like this:

``` haskell
type family ValidEncoding (k :: Symbol) :: Bool where
  ValidEncoding "RETRIEVED_ENCODING_1" = 'True
  ValidEncoding "RETRIEVED_ENCODING_2" = 'True
  ...
```

Note two things: we are using a _closed_ type family to avoid "monkey patching" of our encodings (something which
could happen if we chose a typeclass as an abstraction mechanism, as someone could have defined an orphan instance) and
we "plug" directly each retrieved encoding as a string literal. So far so good! The "magic" between the minimal API lies
in this few lines of code:

``` haskell
type Enc k1 k2 = ByteString

--------------------------------------------------------------------------------
convert :: forall k1 k2. ( KnownSymbol k1
            , KnownSymbol k2
            , ValidEncoding k1 ~ 'True
            , ValidEncoding k2 ~ 'True
            )
         => Enc (k1 :: Symbol) (k2 :: Symbol) -- ^ Input text
         -> ByteString -- ^ Output text
convert input = I.convert (reifyEncoding (E @k1)) (reifyEncoding (E @k2)) input
```

First of all, we define a type synonym called `Enc` with 2 phantom types, which will be "filled"
by our encodings. This unfortunately generate ambiguity and GHC reports this at compile time. We can
help the ambiguity resolving by using `AllowAmbiguousTypes`, which basically 
(check this [insigthful comment](https://www.reddit.com/r/haskell/comments/59g49o/iconvtyped_an_experiment_in_api_design_and_type/d99pnhx/) 
on Reddit for the full explanation. Thanks `/u/int_index`!)

The `convert` function has a bit of an intimidating, so let's start from the typeclass constraints: what
I'm saying here is that for any genering `k1` and `k2` I want those to:

* Be an instance of `KnownSymbol` (think about a `String` at the type level) so I can reify them back
at the value level with `reifyEncoding` (which is basically just `symbolVal` under the hood).

* The type-level function `ValidEncoding` must yield `True`. Simply put, this will only be possibly with
the `Symbol`s I have defined an instance for in my closed type family. This is what will prevent you from
passing an input a non-existing or mispelled encoding.

The input `ByteString` is well, just a `ByteString` in disguise. Remember `Enc`? That's basically it, with
the only twist of carrying these 2 extra types around, which I'm also saying they are of _type_ `Symbol`,
and this is where I need `TypeInType`, as `Symbol` would normally be a `Kind`.

This is the gist of it! But why am I able to invoke the `convert` function like this?

``` haskell
main = print $ convert @"UTF-8" @"LATIN1" "hello"
```

Here is `TypeApplications` in action! What we are doing is giving an hint to the compiler about which types
are `k1` and `k2`, as the only _real_ input is the input `Enc k1 k2`. Other way to see this, is that we
are saying "Hey GHC, `Enc` carries the utterly generic & ambiguous `k1` and `k2`, I'm telling you explicitly
what those 2 are".

That's pretty much it, really!

## Usability: the unknown

Something I still have no clue is how practical to use this library will be, mostly because those encodings
don't exist at the value level. But not only that, is also very likely you have some existing code which is
doing any kind of manipulation with the `EncodingName`, like comparing them, reading them from disk, from
user input, etc. After all, they are only `String`s. I suspect doing the equivalent with this library will
be clunky, although I hope `TypeInType` can help in this regard.

## You said API design?

The current API is the result of multiple iterations. Initially I was going to use a much simpler approach and simply
have my TH code generate plain types so that our API could look like:

``` haskell
convert UTF8 LATIN1 "hello"
```

This was jolly good for the simplest encodings, but I quickly run into limitations in the allowed characters to be used
for a type/type constructor. For example, `-` is not allowed, so I could have the choice of mangling `UTF-8` into `UTF_8`,
which would have been OK. But what about `ISO_646.IRV:1991`?
I quickly realised this approach had 2 problems:

* It would require the user to "lookup" the mangled name of Haddock as I couldn't come up with a mnemonic rule for
translating encoding into types
* Converting original `iconv` code would have been a bit painful.

In my opinion, if you are releasing a library which is meant to simplify user life, you really want to aim for a low
entry barrier!

### Second attempt: Use an ancilliary `E` type

My second attempt is basically what I ended up releasing as the "GHC 7.x" API version. It does work, but when I first
releases the library and I tweeted the link to GitHub, [Anthony Cowley](https://twitter.com/a_cowley/status/790236285847863296)
gave valuable suggestions on how to improve it, which made me realise that if I was going to use `TypeApplications` and
therefore tap into GHC 8.x anyway, I had access to `TypeInType` also! That yielded a much nicer API.

### Final attempt: Perfection?

Exploring the solution space brought me in a place where I feel I have come up with an API which strikes me as a good
compromise. The only sour taste in my mouth is the use of `AllowAmbiguousTypes`, which I wasn't able to avoid.

## Support for GHC 7.x

Althought not as slick and elegant as the version which uses `TypeInType` and `TypeApplications`, we support older versions
of GHC. This is how the API would look like if you try to compile `iconv-typed` with GHC 7.x:

``` haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Text.IConv.Typed

main :: IO ()
main = print $ convert (E :: E "UTF-8") (E :: E "LATIN1") "hello"
```

## Unexplored territory

As a result of my encoding fetching at compile time, there is something which is subtle and with an impact I
cannot anticipate: **If you try to run a program which is using iconv-typed on a machine which doesn't support
a particular encoding, your application won't compile**.

Differently put, if you are doomed to produce garbage as part of your encoding process because your underlying
iconv library doesn't support that particular encoding, the library will prevent you from even trying. This
could certainly be terrifying or beautiful, depending from the point of view.

I guess we will have to wait and see!

## Notes

[1] Using `unsafePerformIO` here is not necessary, using `runIO` in the `Q` monad would have worked as well,
and avoid unsafe operations.
