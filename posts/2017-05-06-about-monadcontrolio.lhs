---
title: About MonadBaseControl
description:
tags: haskell, fp
---

This is a literate Haskell post. You can play with the examples in ghci, in a stack playground, calling:

```
stack ghci --package transformers --package transformers-base --package monad-control --package distributed-process --package distributed-process-monad-control
```

Let's start with some imports:

> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> import Control.Monad.Base
> import Control.Monad.State hiding (StateT, runStateT, execStateT, evalStateT, get)
> import Control.Monad.Trans.Control
> import Control.Monad.Trans.State.Strict
> import Control.Distributed.Process
> import Control.Distributed.Process.MonadBaseControl

Consider this monad:

> data Ctx = Ctx () -- some kind of env, not important.
> newtype RemotePure m a = RemotePure { runRemote :: StateT Ctx m a }
>                          deriving (Functor, Applicative, Monad, MonadState Ctx, MonadIO)
> type RemoteM = RemotePure Process
>
> instance MonadBase IO (RemotePure Process) where
>   liftBase = RemotePure . liftBase

You now would like to write an instance for [MonadBaseControl](https://hackage.haskell.org/package/monad-control-1.0.0.5/docs/Control-Monad-Trans-Control.html#t:MonadBaseControl).

This is the definition of `MonadBaseControl` and `RunInBase` (at the time of writing, April 2017):

``` haskell
class MonadBase b m => MonadBaseControl b m | m -> b where
    type StM m a :: *
    liftBaseWith :: (RunInBase m b -> b a) -> m a
    restoreM :: StM m a -> m a

type RunInBase m b = forall a. m a -> b (StM m a)
```

A legitimate question is: how can I write the correct right hand side for `StM`? As you might know, when the `type` keyword is
being used in a type class definition we are not dealing with a _type synonym_ but with a _type family_. A _type family_ is
essentially a function with operates on _types_, not _values_, like "normal" functions. But how can I pick the correct RHS?
How would I know? There are two approaches: one seems to be the most popular, but it requires the use of `UndecidableInstances`,
the other was found in this small nugget of [wisdom](http://stackoverflow.com/a/33558631/479553)[^1] over at Stack Overflow.

The first one is this:

``` haskell
instance MonadBaseControl IO RemoteM where
  type StM RemoteM a = StM (StateT Ctx Process) a
  liftBaseWith f = RemotePure $ liftBaseWith $ \q -> f (q . runRemote)
  restoreM = RemotePure . restoreM
```

Why this works? Also, it might not be immediately clear why `StM` appears on the right hand side. You might ask yourself
"Did he just pull out the type StM out of thin air and reused it?". It's allright, I have been there myself. Haskell notation
is so dense sometimes it's easy to get lost. The key insigth is this: **StM is NOT a type, is a type-level FUNCTION!** Here,
all we are doing is calling `StM` on the RHS, effectively offloading computing the result and hoping that somebody already
defined in the stack the final solution. So we are effectively applying `StM` to `(StateT Ctx Process) a` as an argument. And
this is exactly _why_ GHC asks us to enable `UndecidableInstances`. It cannot guarantee, without compiling the program, that GHC
will terminate. Effectively (if I recall correctly what Andres Loh once told us in an Haskell course in London), as scary as
the name might sound, `UndecidableInstances` simply tells us "hey, it's probably going to be fine, but there is a chance the
typechecking might not terminate". This is (very loosely speaking) because the RHS doesn't "reduce" as it has the same number
of terms of the LHS, so GHC gets suspicious.

The other approach is to simply ask GHC for the result of the type family. How? Let's fire up `ghci`, and let's type this:

```
ghci> :set -XRankNTypes
ghci> import Control.Monad.Trans.Control
ghci> :kind! forall a. StM RemoteM a
forall a. StM RemoteM a :: *
= (a, Ctx)
```

Wow, can you believe how easy it was? It's equally easy to convince ourselves why this result makes sense: this is nothing
more of the result of applying `Stm` to `StateT Ctx Process`. Let's find out:

```
ghci> import Control.Monad.Trans.State.Strict
ghci> import Control.Distributed.Process
ghci> :kind! forall a. StM (StateT Ctx Process) a
forall a. StM (StateT Ctx Process) a :: *
= (a, Ctx)
```

And the best part is that now we don't need `UndecidableInstances`, and we are much more confident we are computing `StM`
the right way. Our definition becomes:

> instance MonadBaseControl IO RemoteM where
>   type StM RemoteM a = (a, Ctx)
>   liftBaseWith f = RemotePure $ liftBaseWith $ \q -> f (q . runRemote)
>   restoreM = RemotePure . restoreM


[^1]: Thanks, Daniel Wagner!
