---
title: Fast Random Strings Generation in Haskell
description: A fast and handy way to generate a corpus of random strings.
tags: fp, haskell
---

##Fast Random Strings Generation in Haskell
This morning there was an interesting question on Haskell Cafè:

<div markdown="1">
<br>
<em>
Hello anyone,
I've written a snippet which generates a file full of random strings. When
compiled with -O2 on ghc-7.6, the generation speed is about 2Mb per second
which is on par with interpreted php. That's the fact I find rather
disappointing. Maybe I've missed something trivial? Any suggestions and
explanations are welcome.</em>

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
