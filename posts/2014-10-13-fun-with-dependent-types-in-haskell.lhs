---
title: Fun with Dependent Types in Haskell
description: A perhaps-not-so-interesting example
tags: haskell, type-families
---

Inspired by recent [Oliver Charles' talk]() at the
Haskell XChange (which was a blast, btw), I felt
compelled in trying to fiddle around with type families
and dependent types in Haskell to fix the concept in my
long term memory. Even though those weren't now, and
Ollie's talk was very accessible and nicely delivered,
I wanted to brainstorm a bit to see what I could get
out of it. First of all I read the [suggested paper](),
which didn't add anything on the plate but was a really
nice refresher on the topic.

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> module Main where

> import GHC.TypeLits
> 
> -- http://hackage.haskell.org/package/base-4.7.0.1/docs/GHC-TypeLits.html
> 
> type family SillyID k1 :: *
> 
> type instance SillyID Bool = Bool
> type instance SillyID Int = Int
> 
> whyNot :: Bool -> SillyID Bool
> whyNot True = False
> whyNot False = True
> 
> 
> data Vec :: Nat -> * -> * where
>   VNil :: Vec 0 a
>   (:*:) :: a -> Vec n a -> Vec (n + 1) a
> 
> infixr 5 :*:
> 
> type family Not (b :: Bool) :: Bool where
>   Not 'True = 'False
>   Not 'False = 'True
> 
> type family Equally (n :: Ordering) (m :: Ordering) :: Bool where
>   Equally GT GT = True
>   Equally LT LT = True
>   Equally EQ EQ = True
>   Equally x y = False
> 
> type family NonZero (n :: Nat) :: Bool where
>   NonZero x = Equally (CmpNat 0 x) LT
> 
> type family If (pred :: Bool) (thenB :: Bool) (elseB :: Bool) :: Bool where
>   If True t r = t
>   If False t r = r
> 
> posMultOf :: Int -> Int -> Bool
> posMultOf _ 0 = True
> posMultOf x y = if y < 0 then False else posMultOf x (y - x)
> 
> type family PosMultOf (n :: Nat) (m :: Nat) :: Bool where
>   PosMultOf x 0 = True
>   PosMultOf x y = If (Not (NonZero y)) False (PosMultOf x (y - x))
> 
> -- Calling stuff on an empty vector shouldn't typecheck..
> safeHead :: NonZero n ~ True => Vec n Int -> Int
> safeHead (x :*: _) = x
> 
> -- This will compile
> safe1 :: Vec 1 Int -> Int
> safe1 = safeHead
> 
> -- This won't..
> --unsafe1 :: Vec 0 Int -> Int
> --unsafe1 = safeHead
> 
> multOf5 :: PosMultOf 5 n ~ True => Vec n Int -> Int
> multOf5 (x :*: _) = x
> 
> correct :: Vec 5 Int -> Int
> correct = multOf5
> 
> correct' :: Vec 0 Int -> Int
> correct' = multOf5
> 
> correct'' :: Vec 10 Int -> Int
> correct'' = multOf5
> 
> -- Does not typecheck!
> -- bogus :: Vec 7 Int -> Int
> -- bogus = multOf5
> 
> data SBool (b :: Bool) where
>   STrue  :: SBool 'True
>   SFalse :: SBool 'False
> 
> notTF :: SBool b -> SBool (Not b)
> notTF STrue  = SFalse
> notTF SFalse = STrue
