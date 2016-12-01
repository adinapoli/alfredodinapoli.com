---
title: Convince me to use Rust
description: In doubt about which system language learn next
tags: programming, life, rust
---

## Convince me to use Rust

__TL;TR I really like Rust, but I feel overwhelmed by its syntax and
complexity, so I hope the Rust community will sell me the language,
convincing me to learn it.__

As we approach the new year, it seems quite natural to follow this
[list](http://matt.might.net/articles/programmers-resolutions/) of things
I should aim to do in 2015. One of them is learning a new language, which I
feel it's quite an important one. After having a love-hate relationship with
high and low level languages, I'm not in that period of my life where I would
like to learn a new system language. I've done a bit of C/C++ back in
university days, so I know what lies in store, even considering the latest
available standards (i.e. c11, C++11, C++14 and so on). I would like to
learn something *different*, and I went back and forth in deciding whether
I should learn Go or Rust (I know, potentially I should learn both). The
real question is: Which of the two?

## About me
First of all, let me say I'm a [Haskell hacker](http://www.alfredodinapoli.com/oss.html).
Not only am I a OSS contributor, but I'm lucky enough to get paid to code in
Haskell during my everyday job. So I am a firm believer that a strong type
system and a strong compiler really matters in delivering robust software. So,
in a sense, it seems that the natural continuation in my skills development would
be to learn Rust, which gets a **lot** of things right (but I'm sure you didn't
need me to discover this): immutability by default, a sophisticated borrow checker,
ADTs, pattern matching, (limited form of) monads and even HKT (only emulated for now,
hopefully fully supported in Rust 1.0).

## So what?
So what? You might be thinking, which would be a perfectly reasonable feeling.
If you feel Rust is the "next big thing", you should learn it as your next
system language, right?
That's true, but I want to play the devil's advocate here, and I really hope
the Rust community will jump on me and completely sell me the language, so
I will happily hack in it during 2015 (together with Haskell, of course!).

## Zen
If you are not familiar with the [Zen](http://www.amazon.com/Complete-Idiots-Guide-Living-Edition/dp/159257243X) philosophy,
I will definitely suggest you to dig more into it. Zen can be a lot of things, a religion, a way of
living, and a way of coding, too.
What I really appreciate of the Zen culture is that things like "beauty" and
"simplicity" are something which should be researched in everything we do (also "perfection", but
that sounds more like utopia!).
Leaving apart [other kind of Zen manifesto](https://www.python.org/dev/peps/pep-0020/), I'm a strong
believer that beauty in code leads to simplicity, which leads to beauty, which leads to simplicity, which..

Let's take Haskell, for example. Don't you find this is utterly beautiful?

```
fmap :: (Functor f) => (a -> b) -> f a -> f b
```

If you are unfamiliar with Haskell it doesn't matter, all you need to know is that this is the
function signature for the `fmap` function, which can be specialised for lists, to name one data structure.
What I like here is that:

* It's simple, with a minimal syntax
* It's completely generic, where `a`, `b` and `f` are completely parametric
* I can see upfront the "contract" of this function: `f` must be a [functor](http://en.wikipedia.org/wiki/Functor)


Something which puts me off from learning Rust is the "eye bleeding" (perhaps I'm a bit exaggerating here!) I have when I look at certain snippets of Rust code. I feel overwhelmed by the variety of
operators you can use to denote your variables, the macro applications, the trait implementations
and much more. These are just two examples I copied opening two random Rust
projects on Github:

```
/// An abstraction to receive `NetworkStream`s.
pub trait NetworkAcceptor<S: NetworkStream>: Acceptor<S> + Clone + Send {
    /// Closes the Acceptor, so no more incoming connections will be handled.
    fn close(&mut self) -> IoResult<()>;
}

/// An abstraction over streams that a Server can utilize.
pub trait NetworkStream: Stream + Any + StreamClone + Send {
    /// Get the remote address of the underlying connection.
    fn peer_name(&mut self) -> IoResult<SocketAddr>;
}

#[doc(hidden)]
pub trait StreamClone {
    fn clone_box(&self) -> Box<NetworkStream + Send>;
}

impl<T: NetworkStream + Send + Clone> StreamClone for T {
    #[inline]
    fn clone_box(&self) -> Box<NetworkStream + Send> {
        box self.clone()
    }
}
```

```
impl<'c> Cursor<'c> {
    /// Create a new cursor instance
    pub fn new(line: &'c mut Line, offset: uint) -> Cursor<'c> {
        let mut cursor = Cursor {
            offset: offset,
            line: line,
        };

        // check that the current offset is longer than the length of the line
        let offset = cursor.get_offset();
        let line_length = cursor.get_line().len();
        if offset > line_length {
            cursor.set_offset(line_length);
        }
        cursor
    }
}
```

This is obviously very much subjective, but I find Rust code **very** dense; someone could
say the same of Haskell, I suppose, so I'm not sure how much my point stands. But when I look
at Rust, I basically see C++ in disguise (angular brackets everywhere, very dense and
complicated). Having programmed in C++ before, I was really hoping, in a sense, to get a breath
of fresh air.

On the contrary, **Go** seems to be exactly the opposite: I basically call it "C with concurrency".
But it has a strange allure, probably deriving from its simplicity: **I like simple things**.
On the other hand, it goes against my outlook on software development, as is not very "safe",
as far as the compiler and the type checker is concerned. But the visual overhead is much less.

## Conclusions

If you make it till here, you guess this is **not** a flame post. It's just my personal
ruminations in what makes me feel reluctant in spending my time learning Rust. I really
hope people will help me see through the syntax and appreciate the true sprit of the language.
