---
title: Fast Random Strings Generation in Haskell
description: A fast and handy way to generate a corpus of random strings.
tags: fp, haskell
---

##Fast Random Strings Generation in Haskell
Yesterday morning there was an interesting question on Haskell Cafe:

<div markdown="1">
<br>
<em>
Hello anyone,
I've written a snippet which generates a file full of random strings. When
compiled with -O2 on ghc-7.6, the generation speed is about 2Mb per second
which is on par with interpreted php. That's the fact I find rather
disappointing. Maybe I've missed something trivial? Any suggestions and
explanations are welcome.</em>

It seemed an interesting exercise and we can't put up being on par with
PHP, so I decided to dig in. The code was the following:

~~~~~{.haskell}
import qualified Data.Text as T
import System.Random
import Control.Exception
import Control.Monad

import System.IO
import qualified Data.Text.IO as TI

genString g = let (len, g') = randomR (50, 450) g
               in T.unfoldrN len rand_text (len, g')
 where rand_text (0,_) = Nothing
       rand_text (k,g) = let (c, g') = randomR ('a','z') g
                         in Just (c, ((k-1), g'))

writeCorpus file = bracket (openFile file WriteMode) hClose $ \h -> do
  let size = 100000
  sequence $ replicate size $ do
    g <- newStdGen
    let text = genString g
    TI.hPutStrLn h text

main = do
  putStrLn "generating text corpus"
  writeCorpus "test.txt"
~~~~~

</div>
<br>

The first thing I thought was to switch to ```ByteString```: since we don't
need to deal with ```UTF-8``` encoding (the strings to generate were plain
ASCII strings in the range ```a-z```) we can avoid paying the overhead
```Data.Text``` introduces. After that, the code was a slightly faster, but
nothing thrilling (about ```7 seconds``` on my machine).

The crucial hint was gave from Gregory Collins, who suggested that
```System.Random``` was very slow. He proposed, as better alternative, the
[mwc-random](http://hackage.haskell.org/package/mwc-random-0.12.0.1) package,
and the fact [Bos](http://www.serpentine.com/blog/) was the author was a
guarantee. After a bit of struggling I ended up with the following code:

```Haskell
import qualified Data.Text as T
import System.Random.MWC
import Control.Monad
import Control.Monad.Primitive
import System.IO
import Data.ByteString as B
import Data.Word (Word8)
import Data.ByteString.Char8 as CB


{- | Converts a Char to a Word8. Took from MissingH -}
c2w8 :: Char -> Word8
c2w8 = fromIntegral . fromEnum


------------------------------------------------------------------------------
charRangeStart :: Word8
charRangeStart = c2w8 'a'
{-# INLINE charRangeStart #-}


------------------------------------------------------------------------------
charRangeEnd :: Word8
charRangeEnd = c2w8 'z'
{-# INLINE charRangeEnd #-}


------------------------------------------------------------------------------
genString :: Gen (PrimState IO) -> IO B.ByteString
genString g = do
    randomLen <- uniformR (50 :: Int, 450 :: Int) g
    str <- replicateM randomLen $ uniformR (charRangeStart, charRangeEnd) g
    return $ B.pack str


------------------------------------------------------------------------------
writeCorpus :: FilePath -> IO [()]
writeCorpus file = withFile file WriteMode $ \h -> do
  let size = 100000
  _ <- withSystemRandom $ \gen ->
      replicateM size $ do
        text <- genString gen :: IO B.ByteString
        CB.hPutStrLn h text
  return [()]

main :: IO [()]
main =  writeCorpus "test.txt"
```

The are a couple of interesting points:

* As stated inside the docs ```withSystemRandom``` _is a somewhat
  expensive function, and is intended to be called only occasionally (e.g.
  once per thread). You should use the ```Gen``` it creates to generate
  many random numbers_). This explains we have that weird monadic binding
  that binds the result of ```withSystemRandom``` to ```_```: what we are
  doing is confining the ```Gen``` generation is the "inside loop". This
  guarantee that we get a brand new ```Gen``` to pass to each invocation
  of ```genString```.

* I've extrapolated ```c2w8``` from the
  [MissingH](http://hackage.haskell.org/package/MissingH-1.1.1.0)
  library from John Goerzen. We need this because ```uniformR``` expects a type
  which is instance of ```Variate```, which is defined for ```Word8``` but
  not for ```Char```. Last but not least due to the fact I wanted to avoid
  calling ```c2w8``` at each iteration, we can speed things up to inline
  both ```charRangeStart``` and ```charRangeEnd```.

Not only is our code cleaner, but is also a great deal faster! On my machine
generating 10000 words takes more or less *half a second*!

Well, a lot faster than PHP, as expected.
