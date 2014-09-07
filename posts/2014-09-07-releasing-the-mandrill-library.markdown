---
title: ANN: The mandrill package for interfacing with the Mandrill API
description: Releasing a new library on Hackage
tags: haskell
---

# Releasing the "mandrill" package

Today I'm releasing the [mandrill](https://hackage.haskell.org/package/mandrill)
package for interfacing with the
[Mandrill API](https://mandrillapp.com/api/docs/).
In case you don't know what Mandrill is, is an API-based
service for sending transactional emails.
The free plan ships with a free and attractive quota of 12k email a month,
which makes it a cheap solution for small-medium businesses.

# About the mandrill package

The package per se is not rocket science: it just ships a bunch of
boilerplate to be able to send and receive JSON from Mandrill. On top of
this tough:

* It provide decent documentation;
* It [compiles and supports](https://travis-ci.org/adinapoli/mandrill)
  GHC 7.4, 7.6 and 7.8, making it usable in production even if you are
  running a couple of GHC versions behind;
* It ships with a low level IO-based API as well as an handy transformer.

# What has been wrapped

The library scratches my own itches, namely be able to send quickly and
easily an email message using Mandrill. Pull Requests are certainly welcome,
please [fork away](https://github.com/adinapoli/mandrill)!

# Getting started

Using the built-in transformer is as easy as:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Text.Email.Validate
import Network.API.Mandrill

main :: IO ()
main = do
  case validate "foo@example.com" of
    Left err   -> print $ "Invalid email!" ++ show err
    Right addr -> runMandrill "MYTOKENHERE" $ do
      let msg = "Hello from Haskell"
      res <- sendEmail (newTextMessage addr [addr] "Hello" msg)
      case res of
        MandrillSuccess k -> liftIO (print k)
        MandrillFailure f -> liftIO (print f)
```

# The choice of email-validate

Astute readers will notice I have used the [email-validate](https://hackage.haskell.org/package/email-validate) package from Hackage.
Even though I like the fact you are forced to validate
your email before sending them out, I also feel this might become
burdensome on the long run. I'm open to feedback here.

# Getting the package

The package is available on Hackage as we speak:

``` 
cabal install mandrill
```

Enjoy!
Alfredo
