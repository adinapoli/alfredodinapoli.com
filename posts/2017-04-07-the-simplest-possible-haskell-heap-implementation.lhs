---
title: The simplest Haskell Priority Queue implementation I know of
description: Creating a Priority Queue in Haskell is very easy
tags: haskell, data-structures
---

Nothing about what I'm going to say is novel or particularly mind-blowing,
but yet useful, especially on programming competitions websites like HackerRank.
This implementation is shamelessly stolen from [Okasaki's book](http://www.cambridge.org/it/academic/subjects/computer-science/programming-languages-and-applied-logic/purely-functional-data-structures?format=PB&isbn=9780521663502#RLHQsBjwMXbXyUFW.97).

A [Priority Queue](http://algs4.cs.princeton.edu/24pq/) can be easily implemented
in an imperative setting but is not totally obvious how that could efficiently
translate into a functional language, especially in a pure language like Haskell.

See [this blog post](http://typeocaml.com/2015/03/12/heap-leftist-tree/) for another excellent
implementation, but in OCaml (always based on Okasaki).

This is a Literate Haskell post. Let's begin with the usual importing fandango:

> module PriorityQueue where
>
> import Data.List hiding (insert)
>

"Leftist heaps always keeps the left branches of all roots being the longer and in worst case,
they are as long as the right branches. In other word, all right branches of all roots are shortest.
In order to maintain this property, **each node has a rank, which indidates the
length of the path between the node and the right most leaf.**" -- [^1]

> type Rank = Int
>
> data Heap a = Tip | Node {-# UNPACK #-} !Rank a (Heap a) (Heap a) deriving Show
>
> -- Rank: the length of the path between the node and the right most leaf.
> rank Tip = 0
> rank (Node r _ _ _) = r

> fromList :: Ord a => [a] -> Heap a
> fromList [] = Tip
> fromList (x:xs) = foldl' (\hp val -> insert val hp) (singleton x) xs

`makeHeap` is straightforward: we compare the two ranks, and we preserve
the leftist property by setting as the rank the min of the two, and storing
as the right child the smallest (aka with smallest rank) child.

> makeHeap :: a -> Heap a -> Heap a -> Heap a
> makeHeap x a b = if rank a >= rank b then Node (rank b + 1) x a b
>                                      else Node (rank a + 1) x b a
>
> empty :: Heap a
> empty = Tip
>
> singleton :: a -> Heap a
> singleton x = Node 1 x Tip Tip
>
> insert :: Ord a => a -> Heap a -> Heap a
> insert x h = merge (singleton x) h
>
> -- | Merge two heaps together, preserving the leftist property via `makeHeap`.
> -- Runs in O(log n).
> -- "The key insight behind leftist heaps is that two heaps can be merged by
> -- merging their right spines as you would merge two sorted lists, and then
> -- swapping the children of nodes along this path as necessary to restore
> -- the leftist property." -- Okasaki
> merge :: Ord a => Heap a -> Heap a -> Heap a
> merge l Tip = l
> merge Tip r = r
> merge h1@(Node _ x l1 r1) h2@(Node _ y l2 r2) =
>   if x <= y then makeHeap x l1 (merge r1 h2)
>             else makeHeap y l2 (merge h1 r2)
>
> -- | O(1).
> peekMin :: Heap a -> Maybe a
> peekMin Tip = Nothing
> peekMin (Node _ x _ _) = Just x
>
> -- | O(1), but evaluating the second element of the tuple has same complexity
> -- of `merge`.
> extractMin :: Ord a => Heap a -> Maybe (a, Heap a)
> extractMin Tip = Nothing
> extractMin (Node _ x a b) = Just (x, merge a b)

[^1]: http://typeocaml.com/2015/03/12/heap-leftist-tree/
