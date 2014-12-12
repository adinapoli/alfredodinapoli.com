---
title: Announcing (and releasing) the rncryptor-hs library
description: Releasing a new crypto library in the wild.
tags: haskell, fp
---

## Announcing (and releasing) the rncryptor-hs library

Today I'm releasing the very supercalifragilistic release
of the [rncryptor-hs](https://github.com/adinapoli/rncryptor-hs) library.
This library is a reasonably fast implementation of the [RNCryptor](https://github.com/RNCryptor)
encrypted file format by [Rob Napier](http://robnapier.net/). It comes in two flavour:

1. A "oneshot" function which will decrypt the input bytes retaining all the content in memory
2. A streaming interface (using [io-streams](http://hackage.haskell.org/package/io-streams)) for
   efficient decryption

The state of the library is largely **incomplete**, as was hacked in the previous two days and
a half to respond to a real world case scenario I had at work. Currently what's implemented is
only the decryption, there are basically no unit tests (or QuickCheck properties), but the
quickest way for a library to grow is with people contributing to it, this is why I'm announcing
it at a so-early stage.

## Why should I care?

This library seems to be very widespread in the mobile world (especially on the ObjC side of it),
thus if you are developing for iOS and need to encrypt stuff, there is a high chance you gonna
stumble upon it. But when it comes to the server side of things, if you are using Haskell on your
backend, you (were) out of luck. You could either use C++ and expose a C API and call it from your
Haskell code, or use something like Python. There is actually a [Python implementation](https://github.com/RNCryptor/RNCryptor-python), but it reads the entire content to decrypt in memory, a luxury
I could't afford!

At work, we record, transcode and upload on S3 lots of videos, which are our most important
resource. In doing so, we want to protect such media when we transfer them over the network,
namely from our in-house iOS application towards Amazon's S3. We then notify our transcoder
server (written in Haskell) to go and fetch this encrypted video, decrypt it and upload it into
another folder for the real transcoding to begin. So what I needed was a very simple streaming
CLI program to take encrypted bytes from stdin, decrypt them, and send them over to stdout.
Using my library this is actually quite a piece of cake:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypto.RNCryptor.V3
import qualified System.IO.Streams as S
import System.Environment
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  args <- getArgs
    case args of
        key:_ -> decryptStream (B.pack key) S.stdin S.stdout
            _ -> putStrLn "usage: decrypter <key>"
```

The flexibility `io-streams` give us means you are not limited to stdin/stdout, as
you can build `InputStream` and `OutputStream` also from file `Handle`, raw
`ByteString` and much more.

Combined with Amazon's [aws-cli](https://github.com/aws/aws-cli) tools,
which provides streaming copy from and to
S3, I can now download a file from S3, decrypt it and send it back in one pass,
efficiently:

```
aws s3 cp s3://my-bucket/stream - | decrypter mysecretkey | aws s3 cp - s3://my-bucket/new-stream
```

Easy!

## What's left to do
A lot of things actually! On top of my minds:

- HMAC validation
- Testing!
- Encryption
- Make it independent from any streaming libraries, so that you can plug
  your own (conduit, pipes, you name it)

PR are highly encouraged! So XMas is coming, so are your spare time, so consider
improving the Haskell library ecosystem :)

The library is available on [Hackage](http://hackage.haskell.org/package/rncryptor), grab it while it's hot!

Alfredo
