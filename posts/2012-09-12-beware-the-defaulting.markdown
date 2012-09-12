---
title: Beware the defaulting
description: Here I describe an interesting unfolding (no pun) of a code story.
tags: fp, haskell, ocaml
---

It started innocently enough. I was comparing execution times (as I usually
do) between different programming languages. This time was the turn of
OCaml. I've implemented in it the usual Project Euler #5 problem, because
It's easy and computationally not too expensive. This is the OCaml
implementation:

~~~~~{.ocaml}
open Core.Std

let div_pred n seq =
    List.for_all seq (fun x -> Int.(=) (n mod x) 0)

let solver =
    let seq = List.range ~stride:(-1) 20 1 in
    let rec aux accum =
        match (div_pred accum seq) with
        | true -> accum
        | false -> aux (accum + 20)
    in aux 20

let _ = Printf.printf "%d" solver
~~~~~

It's nice also fast:

~~~~~{.shell}
232792560
./euler5  0,25s user 0,01s system 97% cpu 0,275 total

~~~~~

This was an old version I implemented in Haskell, focusing on
expressiveness:

~~~~~{.haskell}
import Data.List

solver = find (\x -> divPred x) [20,40..]

divPred x = all (\y -> x `mod` y == 0) [20,19..2]

main = print $ solver
~~~~~

Then I executed it:

~~~~~{.shell}
Just 232792560
./euler5  0,84s user 0,05s system 95% cpu 0,934 total
~~~~~

Uhm.. 3 times slower then the OCaml version, must be the overhead
introduced by list lazyness, I thought, because I'm creating an 
infinite list and asking to it for the next value until I find one
that respect my predicate. So I said "_Let's try removing the infinite
list_" and I produced this:

~~~~~{.haskell}
solver :: (Integral a) => a -> [a] -> a
solver n lst = if divPred n lst then n else solver (n + 20) lst

divPred :: (Integral a) => a -> [a] -> Bool
divPred x = all (\y -> x `mod` y == 0)

main :: IO ()
main = print $ solver 20 [20,19..2]
~~~~~

I've also added the signature to aid the compiler with type inference.
This version is slightly faster:


~~~~~{.shell}
232792560
./euler5  0,62s user 0,11s system 96% cpu 0,761 total
~~~~~

But still slower than OCaml's version. I was beginning to struggle with
bang pattern and seq wizardry when I notices a warning by **ghc-mod**:

<div align="center" markdown="1">
Defaulting the following constraint(s) to type `Integer'
</div>

It was referring to the last list, the one in the **main** function. So
I thought "_Let's restrict it's type to Int. After all, Integer allows
numbers to be very huge, and you pay for this feature. Furthermore, I'm
sure that my solution will be in the Int type range_". This is the
final version:

~~~~~{.haskell}
solver :: (Integral a) => a -> [a] -> a
solver n lst = if divPred n lst then n else solver (n + 20) lst

divPred :: (Integral a) => a -> [a] -> Bool
divPred x = all (\y -> x `mod` y == 0)

main :: IO ()
main = print $ solver 20 ([20,19..2] :: [Int])
~~~~~

With my huge surprise, this runs as fast as the OCaml version, paying 
almost no overhead for the (little) lazy list:

~~~~~{.shell}
232792560
./euler5  0,28s user 0,02s system 99% cpu 0,305 total
~~~~~

<div align="center" markdown="1">
**Lesson learned. Beware the defaulting.**
</div>
