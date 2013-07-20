---
title: Slick file download with Haskell in 40 SLOC
description: Downloading a file in Haskell in 40 SLOC
tags: haskell, fp
---

## Slick file download with Haskell

Lately I've submitted a [patch](https://github.com/tmhedberg/hsenv/pull/32)
to Hsenv featuring the ability to download a file over HTTP without
external tools (e.g. curl), but using nothing more than awesome Haskell.
I've posted the cool snippet on Reddit, and someone asked for a version with a
progress bar, similar to what have been implemented in Python here:

[Download a file with Python - SO](http://stackoverflow.com/a/22776)

I came up with a nice version which does rudimental error handling
over HTTP response codes, which implements a progress bar and that
has a constant memory usage, because we still retain all the benefits
of programming with streaming libraries, namely
[io-streams](http://hackage.haskell.org/package/io-streams) and
[http-streams](http://hackage.haskell.org/package/http-streams).
Without further ado, let me show you the code:

``` haskell
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where 
 
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S
import Network.Http.Client
import Network.Socket
import Text.Printf (printf)
 
withProgressBar :: Integer
                -> InputStream ByteString
                -> OutputStream ByteString
                -> IO ()
withProgressBar fileSize inS outS = go (0 :: Int)
  where
    go blocksRead = do
      block <- S.read inS
      case block of
        (Just d) -> do
            let currentBlocks = blocksRead + B.length d
            let percentage = fromIntegral (currentBlocks * 100) /
                             fromIntegral fileSize
            printf "%10d [%3.2f%%]\r" currentBlocks (percentage :: Double)
            S.write (Just d) outS >> go currentBlocks
        Nothing -> return ()
 
downloadFile :: URL -> FilePath -> IO ()
downloadFile url name = withSocketsDo $ get url $ \response inStream ->
    case getStatusCode response of
      200 -> let fileSize = maybe 0 (\fs -> read (C8.unpack fs) :: Integer)
                            (getHeader response "Content-Length")
        in S.withFileAsOutput name (withProgressBar fileSize inStream)
      code -> error $ "Failed to download " ++
                      name ++
                      ": http response returned " ++
                      show code
 
main :: IO ()
main = do
  let url = "http://audacity.googlecode.com/files/audacity-macosx-ub-2.0.3.dmg"
  print $ "Downloading " ++ url
  downloadFile (C8.pack url) "audacity.dmg"
```

As you can see, it's less than 40 lines of code! (ok, a bit more, but just
because I've splitted the type signature and the error message on multiple 
lines to make it fit the blog template). Even better, as someone suggested
on Reddit, we can make this code multi-platform using `withSocketsDo` before `get`.
We need this on Windows machines only (to initialize the underlying socket), but due
to the fact `withSocketsDo` is implemented as `id` on *nix platforms, everything will
just work. Once again, Haskell rocks.
