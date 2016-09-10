---
title: Releasing the threads-supervisor library
description: There is a new library in town 
tags: haskell, life
---

# Releasing the threads-supervisor library
I'm happy to announce the first release of [threads-supervisor](https://github.com/adinapoli/threads-supervisor),
a small library I have extracted from some code I wrote at work (thanks to Iris Connect for allowing me to
release it). The library itself does only one thing: it allows you to fork an IO computation in a supervised fashion,
restarting it in case of failure. In a sense, the library it's similar in spirit to Erlang's OTP approach
to process supervision and supervision trees.  At the moment, we support only one restart strategy, Erlang's `OneForOne`,
which basically means "please always restart this thread". Of course, threads-supervisor is not as feature complete
as the OTP counterpart, nor it aims to be.

# Why not use `distributed-process`, `immortal`, `async`, `slave-threads`, `yet-another-library`?

The aim of this small paragraph is not to convince you that my library the best in town (it's not!),
but more to justify my thought process behind deciding to write it.
When I looked at `distributed-process`, it was clear that it was offering exactly this kind of supervision
and much, much more. The problem is the library is certainly geared towards Cloud Haskell and the idea
of the distributed closures, therefore if you want to use it, you have to buy the full package.
What I wanted, instead, was a simple library, with minimal dependencies, which could be used as a replacement
of `forkIO`, with minimal fuss.

`immortal` is a very nice library indeed, but I also wanted built-in event logging with opt-in subscription,
as well as the possibility of compose my supervisors into a nice supervision tree.

The same sort of reasoning can be generalised; the available library in the ecosystem where close enough to what I wanted
but not **exactly** what I wanted. Therefore, I decided it was just simpler to whip up my small abstraction on top
of the concurrency primitives.

# Using the library

Extensive documentation can be found reading the [tutorial](http://hackage.haskell.org/package/threads-supervisor-1.0.1.0/docs/Control-Concurrent-Supervisor-Tutorial.html),
but I'm going to report here the relevant passages.

Use `threads-supervisor` if you want the "poor-man's Erlang supervisors".
`threads-supervisor` is an IO-based library  with minimal dependencies
which does only one thing: It provides you a 'Supervisor' entity  you can use
to monitor your forked computations. If one of the managed threads dies,
you can decide if and how to restart it. This gives you:

  * Protection against silent exceptions which might terminate your workers.
  * A simple but powerful way of structure your program into a supervision tree,
    where the leaves are the worker threads, and the nodes can be other
    supervisors being monitored.
  * A disaster recovery mechanism.

Who worked with Haskell's concurrency primitives will be surely familiar
with the `forkIO` function, which allow us to fork an IO computation in a separate
green thread. `forkIO` is great, but is also very low level, and has a
couple of subtleties, as you can read from this passage in the documentation:

```
The newly created thread has an exception handler that discards the exceptions
`BlockedIndefinitelyOnMVar`,`BlockedIndefinitelyOnSTM`, and `ThreadKilled`,
and passes all other exceptions to the uncaught exception handler.
```

To mitigate this, we have a couple of libraries available, for example
[async](http://hackage.haskell.org/package/async) and [slave-threads](http://hackage.haskell.org/package/slave-thread).

But what about if I do not want to take explicit action, but instead specifying upfront
how to react to disaster, and leave the library work out the details?
This is what this library aims to do.

In this example, let's create four different threads:

``` haskell
job1 :: IO ()
job1 = do
  threadDelay 5000000
  fail "Dead"
```

This job will die after five seconds.

``` haskell
job2 :: ThreadId -> IO ()
job2 tid = do
  threadDelay 3000000
  killThread tid
```

With this other job instead, we wait three seconds, and then kill a target thread,
generating an asynchronous exception.

``` haskell
job3 :: IO ()
job3 = do
  threadDelay 5000000
  error "Oh boy, I'm good as dead"
```

This guy is very similar to the first one, except for the fact `error` is used instead of `fail`.

``` haskell
job4 :: IO ()
job4 = threadDelay 7000000
```

`job4` is what we wish for all our real-world functions: smooth sailing.
These jobs represent a significant pool of our everyday computations in the IO monad.

## Creating a SupervisorSpec
A 'SupervisorSpec' simply holds the state of our supervision, and can be safely shared
between supervisors. Under the hood, both the `SupervisorSpec` and the `Supervisor`
share the same structure; in fact, they are just type synonyms:

``` haskell
type SupervisorSpec = Supervisor_ Uninitialised
type Supervisor = Supervisor_ Initialised
```

The important difference though, is that the `SupervisorSpec` does not imply the creation
of an asynchronous thread, which the latter does. To keep separated the initialisation
of the data structure from the logic of supervising, we use GADTs and type synonyms to
force you create a spec first. Creating a spec it just a matter of calling `newSupervisorSpec`.

## Creating a Supervisor
Creating a 'Supervisor' from a 'SupervisionSpec', is as simple as calling `newSupervisor`.
Immediately after doing so, a new thread will be started, monitoring any subsequent IO actions
submitted to it.

## Supervising some threads
Let's wrap everything together into a full blown example:

``` haskell
main :: IO ()
main = bracketOnError (do
  supSpec <- newSupervisorSpec

  sup1 <- newSupervisor supSpec
  sup2 <- newSupervisor supSpec

  sup1 `monitor` sup2

  _ <- forkSupervised sup2 OneForOne job3

  j1 <- forkSupervised sup1 OneForOne job1
  _ <- forkSupervised sup1 OneForOne (job2 j1)
  _ <- forkSupervised sup1 OneForOne job4
  _ <- forkIO (go (eventStream sup1))
  return sup1) shutdownSupervisor (\_ -> threadDelay 10000000000)
  where
   go eS = do
     newE <- atomically $ readTBQueue eS
     print newE
     go eS
```

What we have done was spawning our supervisors out from a spec,
and using our swiss knife `forkSupervised` to spawn four supervised
IO computations. As you can see, if we partially apply `forkSupervised`,
its type resemble `forkIO`'s one; this is by design, as we want to keep
this API as IO-friendly as possible.

In the very same example, we also create another supervisor
(from the same spec, but you can create a separate one as well)
and we ask the first supervisor to monitor the second one.

Each `Supervisor` gives you access the its internal event stream, retrievable,
under the form of a `TBQueue`, by calling `eventStream`.

If you run this program, hopefully you should see on stdout
something like this:

```
ChildBorn ThreadId 62 2015-02-13 11:51:15.293882 UTC
ChildBorn ThreadId 63 2015-02-13 11:51:15.293897 UTC
ChildBorn ThreadId 64 2015-02-13 11:51:15.293904 UTC
ChildDied ThreadId 61 (MonitoredSupervision ThreadId 61) 2015-02-13 11:51:15.293941 UTC
ChildBorn ThreadId 65 2015-02-13 11:51:15.294014 UTC
ChildFinished ThreadId 64 2015-02-13 11:51:18.294797 UTC
ChildDied ThreadId 63 thread killed 2015-02-13 11:51:18.294909 UTC
ChildDied ThreadId 62 Oh boy, I'm good as dead 2015-02-13 11:51:20.294861 UTC
ChildRestarted ThreadId 62 ThreadId 68 OneForOne 2015-02-13 11:51:20.294861 UTC
ChildFinished ThreadId 65 2015-02-13 11:51:22.296089 UTC
ChildDied ThreadId 68 Oh boy, I'm good as dead 2015-02-13 11:51:25.296189 UTC
ChildRestarted ThreadId 68 ThreadId 69 OneForOne 2015-02-13 11:51:25.296189 UTC
ChildDied ThreadId 69 Oh boy, I'm good as dead 2015-02-13 11:51:30.297464 UTC
ChildRestarted ThreadId 69 ThreadId 70 OneForOne 2015-02-13 11:51:30.297464 UTC
ChildDied ThreadId 70 Oh boy, I'm good as dead 2015-02-13 11:51:35.298123 UTC
ChildRestarted ThreadId 70 ThreadId 71 OneForOne 2015-02-13 11:51:35.298123 UTC
```

# Conclusions
I hope that you are now convinced that this library can be of some use to you!
It's on Hackage, play with it!

Alfredo
