---
title: Recursion Schemes, an engineering perspective
description: Yet another recursion schemes tutorial
tags: fp, haskell, 2018
katex: on
---

Preface
=======

There ^[1] is ^[2] an ^[3] abundance ^[4] of blog posts on _recursion schemes_
on the internet, and I have posted a bunch of them at the end of this blog post.
They are excellent resources, and perhaps more comprehensive than this entire article,
which aim is not to go into a deep dissertation of recursion schemes, but rather give an
hopefully-approcheable intuition from an engineering perspective, motivating why
they are an interesting concept and why you might care.

I am not a mathematician and some of the parts of this article are pure hand
waving. That's allright, what I want to focus on is the engineering aspect, not
the rigorous mathematical terms. For the interested, there are links at the end
of the article which points to the proofs & other good things.

What they are, and what you should care
=======================================

_Recursion schemes_ are a way to separate recursion from the data structure
recursion is being applied to, and offers an alternative to high-performant traversals
on recursive data structures, as GHC is able to inline away most of the intermediate structures.

Let's take as an example our old dear friend, a `list`, as defined in:

> data List a = Nil
>             | Cons a (List a)

And let's imagine having a function which converts from our alternative
representation to the standard list type:

> toList :: List a -> [a]
> toList Nil = []
> toList (Cons x xs) = x : toList xs

Surely enough, this works:

``` haskell
toList (Cons 1 (Cons 2 (Cons 3 Nil)))
```

However, in order to write our `toList` function we had to explicitly recurse
over our data structure. Wouldn't be nice if we could focus solely on the data
being manipulated? Another example: Let's imagine we want to compute the
sum of this list, as in a specialised _fold_, so to speak:

> listSum :: Num a => List a -> a
> listSum Nil = 0
> listSum (Cons x xs) = x + listSum xs

And again:

```
listSum (Cons 1 (Cons 2 (Cons 3 Nil)))
```

As we will see in a minute, we can provide an alternative way where we specify only how
to compute the result we are interested in, and the recursion is "invisible".

F-algebras and fixed points
===========================

Turns out we can do quite a few cool tricks exploiting what is called an
_F-algebra_ and a _fixed point_. An _F-algebra_ is simply, given an _endofunctor_
_F_, a _morphism_:


$$
\begin{aligned}
F a \longrightarrow a
\end{aligned}
$$

Simplifying all the category theory naming, in our context this translates into
a function (usually called the _evaluator_) which "squash" a functor `F a` into
its _carrier type_ `a`):

>
> type Algebra f a = f a -> a
>

Our `List` is indeed a functor (can trivially be made so) and a possible
evaluator is our `sumList` function:

```
myAlgebra :: Algebra List Int
myAlgebra = listSum
```

But what does this buy us? Apparently nothing, but here is where it starts to
become cool. It turns out F-algebras forms a _category_ and that is possible
to come up with the following diagram (**handwaving alert number 1!**):

$$
\Large F a \xrightarrow{\enspace fmap_m \quad} F b \newline
\qquad \downarrow{\small evalA} \qquad \qquad \Large \downarrow{\small evalB} \newline
a \xrightarrow{\quad \quad m \quad \quad} b \newline
$$

Where `m` is an evaluator which can map from `a` to `b`, `evalA` the evaluator
for the algebra `F a -> a`  and `evalB` the evaluator for the algebra
`F b -> b`.

A digression on fixed points
============================

Fixed points can be thought as the "center of a cyclone", something that
remains the same even if the rest around is moving. Less vaguely, it conceptually
models the fact that for some categories of endofunctors, applying such
endofunctor to a value it won't change the overall result. It's a bit like what
happens when one hit the `cos` button on a calculator. After a while the result
won't change anymore, and that's the fixed point for that function ^[5]. We can write
this in Haskell as:

>
> newtype Fix f = In { unFix :: f (Fix f) }
>

It turns out that there is a mathematical proof that shows that we can rewrite
the following into:

$$
\Large f (Fix f) \xrightarrow{fmap_m} F b \newline
\quad \enspace \downarrow{\small In} \qquad\enspace \Large \downarrow{\small evalB} \newline
Fix f \xrightarrow{\qquad \quad m \quad \medspace} b \newline
$$

And thanks to what's known as the Lambek's theorem, there is an isomorphism
between `f (Fix f)` and `Fix f`, allowing us to reverse the arrow, yielding:

$$
\Large f (Fix f) \xrightarrow{fmap_m} F b \newline
\quad \enspace \uparrow{\small unFix} \qquad\enspace \Large \downarrow{\small evalB} \newline
Fix f \xrightarrow{\qquad \quad m \quad \medspace} b \newline
$$

Here comes the cool part: if you look at this diagram, it's saying that if we
don't know `m`, we can "land" in the same place starting from `Fix f` just by
following the arrows which goes from our `Fix f` up to `b`. If we rename
things a little bit, we end up with the definition of what is called a _catamorphism_:

> cata :: Functor f => (f a -> a) -> Fix f -> a
> cata alg = alg . (fmap (cata alg)) . unFix

This should give you your first "a-ha" moment: we have found a way to encode
recursion outside our algebra, so that we can compute things on our algebra
in a generic way. Let's try this out. Let's recall:

> myAlgebra :: Algebra List Int
> myAlgebra = listSum

In order to be able to use this in a generic way, we need to "factor out" the
recursion out of our data type. In order terms, we need a new type parameter
and to get rid of the recursive call to `List a` in `Cons`:

> data ListF e recur = NilF
>                    | ConsF e recur
> 
> instance Functor (ListF e) where
>     fmap _ NilF = NilF
>     fmap f (ConsF x cc) = ConsF x (f cc)

And let's now write this in terms of `cata`:

> fixedList :: Fix (ListF Int)
> fixedList = In (ConsF 1 (In (ConsF 2 (In (ConsF 3 (In NilF))))))
>
> listSumF :: Algebra (ListF Int) Int
> listSumF NilF = 0
> listSumF (ConsF x n) = n + x
> 
> toListF :: Algebra (ListF Int) [Int]
> toListF NilF = []
> toListF (ConsF x n) = x : n

```
cata listSumF fixedList
```

And similarly for our `toList` implementation:

```
cata toListF fixedList
```

Exploring duality
=================

Duality is a recurring (no pun intended!) concept in category theory, and is
no mystery that is possible to obtain interesting results by merely "flipping
the arrows", so to speak. If we do so, we get what is known as a _coalgebra_:

$$ a -> F a $$

which can be seen as a way to "lift" a value `a` into the endofunctor `F`. In
Haskell terms:

>
> type Coalgebra f a = a -> f a
>

Unsurprisingly, if we flip the arrows in the diagram above, we obtain:

$$
\Large f (Fix f) \xleftarrow{fmap_m} F b \newline
\quad \enspace \downarrow{\small In} \qquad\enspace \Large \uparrow{\small coalg} \newline
Fix f \xleftarrow{\qquad \quad m \quad \medspace} b \newline
$$

Which means we can construct values embellished by `Fix` by simply apply our
coalgebra and then following the arrows. This leads to the definition of `m`,
more commonly known as an `anamorphism`:

>
> ana :: Functor f => (a -> f a) -> a -> Fix f
> ana coalg = In . fmap (ana coalg) . coalg
>

What can we use `ana` for? Unsurprisingly, to build what we called `fixedList`:

>
> mkList :: Coalgebra (ListF Int) [Int]
> mkList []     = NilF
> mkList (x:xs) = ConsF x xs
>

and we can indeed verify the two are equivalent:

```
cata toListF (ana mkList [1,2,3])
```

Putting everything together: hylomorphisms
==========================================

It turns out the pattern of constructing something (via `ana`) and then
folding it via `cata` is common enough that there is a handy combinator we can
use to perform the two operations in a gulp, and is commonly known as an
`hylomorphism`:

>
> -- type Algebra f a = a -> f a
> -- type Coalgebra f b = f b -> b
> hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
> hylo alg coalg = cata alg . ana coalg
>

Computing the balance within a block
====================================

TODO: We can create blocks by supplying a coalgebra and then collaps it to
compute the balance of all transactions in a block.


References & where to go from here
==================================

[1] Patrick Thomson - "Introduction to Recursion Schemes" https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/
[2] Florian Hofmann - "Don't fear the cat-amorphism (nor the hylomorphism)" http://fho.f12n.de/posts/2014-05-07-dont-fear-the-cat.html
[3] John Wiegley - "A Win for Recursion Schemes" http://newartisans.com/2018/04/win-for-recursion-schemes/
[4] Bartosz Milewski - "F-Algebras" https://bartoszmilewski.com/2017/02/28/f-algebras/
[5] Matt Parsons - "Grokking Fix" http://www.parsonsmatt.org/2016/10/26/grokking_fix.html
