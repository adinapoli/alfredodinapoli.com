---
title: State is Pure
description: How can I sleep at night knowing State is pure
tags: fp, haskell
---

Maybe (or maybe not) it happened to you at least once during your Haskell
journey to look at functions like this...

``` haskell
stackManip :: State Stack Int
stackManip = do
  push 8
  pop
  pop
```

...and to think: "How can possibly this work?" In this post I'll desugar the ```State```
monad up to a point where all the other monads will go crazy screaming "Oh my God,
it's naked!". But let's start from the foundations.

## State monad at the speed of light
The aim of this post is not teaching you how to use the ```State``` monad, I assume
you are already familiar with it. Let's first recall:

``` haskell
instance Monad (State s) where
  return x = State $ \s -> (x,s)
  m >>= f = State $ \s ->
              case runState m s of
                (x, s') -> runState (f x) s'
```

This will be useful later on. For demonstration purposed I'm going to use a
shameless copy of the stack examples you'll find in
[Learn You a Haskell](http://learnyouahaskell.com/for-a-few-monads-more),
because it's easy to grasp and perfect for our purposes. Let's define a ```Stack```
as a type synonym for a list of ```Int```, plus to stateful computations to push
and pop stuff to/from the stack:

``` haskell
type Stack = [Int]

push :: Int -> State Stack ()
push x = state $ \xs -> ((), x:xs)

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)
```

Armed with these two, we can write another stateful computation that modify
the stack:

``` haskell
stackManip :: State Stack Int
stackManip = do
  push 8
  pop
  pop
```

And finally run the example:

``` haskell
main :: IO ()
main = print . show $ runState stackManip [1,2,4,9]
```

This works. But how? In other terms, when I was a beginner Haskeller, this
was the question I kept repeating to myself "Where is the state (the stack) passed around?"
From my personal experience this often confuses newcomers, which might expect
every function dealing with the state to take in input the "old" state in order
to modify it. In other terms, this is what the newcomer expects:

``` haskell
push' :: Stack -> State Stack ()
push' = undefined --not implemented
```

And the fact that he sees just a stateful computation instead, without any
"access point" for the old state to be passed in, confuses him. I'm going to 
shed light exactly over this.

## The example, desugared
Let's get back to our main for a moment:

``` haskell
main :: IO ()
main = print . show $ runState stackManip [1,2,4,9]
```

In order to yield a result, we need to evaluate ```stackManip```, so let's take
a look at it, again (_repetita iuvant_):

``` haskell
stackManip :: State Stack Int
stackManip = do
  push 8
  pop
  pop
```

We can easily rewrite the function without the do block, this way:

``` haskell
stackManip' :: State Stack Int
stackManip' = push 8 >> pop >>= \_ -> pop
```

We can now substitute the functions which their actual implementations:

``` haskell
stackManip'' :: State Stack Int
stackManip'' = (state $ \s -> ((), 8:s)) >>
               (state $ \(x:xs) -> (x,xs)) >>= \_ ->
                state $ \(x':xs') -> (x',xs')
```

Here I've already made a simplification: we already know what the new state
for ```push 8``` will be: exactly the same we had at he beginning plus "8" on
the top of the stack.

Now it's time to recall how ```(>>)``` is defined in Haskell:

``` haskell
(>>) :: m a -> m b -> m b
m1 >> m2 = m1 >>= \_ -> m2
```

Which yield the following:

``` haskell
stackManip''' :: State Stack Int
stackManip''' = ((state $ \s -> ((), 8:s)) >>= \_ ->
                (state $ \(x:xs) -> (x,xs))) >>= \_ ->
                 state $ \(x':xs') -> (x',xs')
``` 

If you look carefully, I've grouped on purpose the first two stateful
computation, to stress the fact we are going to desugar something like this:

``` haskell
(op1 >>= op2) >>= op3
```

And now the fun begins: we need to desugar even further ```(>>=)```, with the
definition I gave you at the beginning of the article:

``` haskell
stackManip'''' :: State Stack Int
stackManip'''' = (state $ \k ->
  case runState m k of
    (x, s') -> runState (f x) s') >>= \_ ->
  state $ \(x':xs') -> (x',xs')
  where
    m = state $ \s -> ((), 8:s)
    f _ = state $ \(x'':xs'') -> (x'', xs'')
```

The only thing I've done here to enforce readability is to move the two
stateful computations we want to bind into two separate expressions, giving
them a nomenclature similar to the familiar one (```m >>= f```).
Let's substitute further, getting rid of the case and of the first ```runState```.
I'm going to do two passages at once, but it's mechanical if you follow the
original implementation of ```(>>=)```:

``` haskell
sm' :: State Stack Int
sm' = (state $ \k ->
       runState (state $ \(x:xs) -> (x, xs)) (8:k)) >>= \_ ->
       state $ \(x'':xs'') -> (x'', xs'')
```

Let's simplify a bit the expression in the middle: ```state``` and ```runState```
and inverse of each other:

``` haskell
sm'' :: State Stack Int
sm'' = (state $ \k -> (8,k)) >>= \_ ->
        state $ \(x:xs) -> (x,xs)
```

Much leaner! But now, unfortunately we're going to beef up this once again,
to get rid of the second bind. But hang thigh, once we do this we'll have our
final result:

``` haskell
sm''' :: State Stack Int
sm''' = state $ \q ->
         case runState (state $ \k -> (8,k)) q of
           (x,s') -> runState (f x) s'
  where f _ = state $ \(x':xs') -> (x',xs')
```

It should not come as a surprise what I've done here: I've just applied again
the definition of ```(>>=)```, and moved the function to bind into a separate
expression. It's not time to simplify everything the way we did before (namely
getting rid of the first ```runState``` and ```case``` expression):

``` haskell
sm'''' :: State Stack Int
sm'''' = state $ \q -> runState (state $ \(x:xs) -> (x,xs)) q
```

And finally simplifying that expression we yield our final computation!

``` haskell
smFinal :: State Stack Int
smFinal = state $ \(q:qs) -> (q,qs)
```

Well, this was somewhat expected, because pushing something and popping twice
is equivalent to just pop once from the initial stack! Notice what happened:
we started with a handful of "actions", and we mechanically boiled them down
to a single value! It's now straightforward to see how this can work:

``` haskell
main = print . show $ runState smFinal [1,2,4,9]
```

All we do is to extract a function from our ```State``` (with ```runState```)
and apply the underlying function to the initial stack. Nothing magical at all!

## Do I need to worry about all this when writing my programs?
I would say "hell no"! Is the abstraction which makes Haskell the wonderful
language it is, so the compiler already does the "menial" job for you. All you
need to do is to write your stateful computations with the idea that whatever
you'll write in a do block will be "sequenced" together and the state will
be passed around as "functions applications".

## Conclusions
I hope I was able to convey several messages:

* ```State``` is pure, it's just function application all over the place, in a 
  CPS fashion.
* You can't write functions which takes the old state and yield a new stateful
  computation, because the state is already "trapped" in the stateful computation
  itself, and eventually all that function applications will boil down to a 
  single function which takes your **initial** state and modify it as the
  computation proceeds.
