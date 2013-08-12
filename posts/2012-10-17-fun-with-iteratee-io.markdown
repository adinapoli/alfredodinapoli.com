---
title: Fun with Iteratee IO
description: How many occurrences of Adenine a DNA Genome has?
tags: fp, haskell
---

Iteratee IO is an IO technique popularized by
[Oleg Kiselyov](http://okmij.org/ftp/Streams.html#iteratee), who gives
the following definition inside his website:

<div class="glance-box" markdown="1">
Iteratee IO is a style of incremental input processing with precise resource
control. The style encourages building input processors from a user-extensible
set of primitives by chaining, layering, pairing and other modes of
compositions. The programmer is still able, where needed, to precisely control
look-ahead, the allocation of buffers, file descriptors and other resources.
The style is especially suitable for processing of communication streams, large
amount of data, and data undergone several levels of encoding such as pickling,
compression, chunking, framing. It has been used for programming
high-performance (HTTP) servers and web frameworks, in computational
linguistics and financial trading.
</div>
<br>

Sounds appealing, isn't it? If you would like to go further, I suggest you
take a look to his excellent paper
[here](http://okmij.org/ftp/Haskell/Iteratee/describe.pdf).

### The gist of Iteratee IO
Probably I'm not the right person for explaining what Iteratee IO is about,
and the linked paper does a better job than me, so I warmly recommend you
read it. In layman's terms, the entire bulk of the Iteratee IO gravitates
around three main concepts:

* An ```Enumerator```, which is an entity that handle our resource in a
  convenient way.

* An ```Enumeratee```, which is a "stream adapter" capable of changing
  the characteristics of our stream. A typical ```Enumeratee```, for
  example, could receive as input a ```Text``` stream and produce as
  output a ```ByteString``` stream.

* An ```Iteratee```, which is where our computation actually happens.

Probably the whole concept is a great deal more complex than what I told
you here, but is all you need to begin working with Iteratee IO, in my
opinion.

```Haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Enumerator (Iteratee, (==<<))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import System.Environment


countCharBS :: (Monad m) => Char -> Iteratee ByteString m Integer
countCharBS needle = loop 0
  where loop n = EL.head >>= check n
        check n Nothing = return n
        check n (Just t) = 
          let !acc = (n + toInteger (B.count needle t)) 
          in loop acc


main :: IO ()
main = do
    args <- getArgs
    case args of
      (fname:needle:[]) ->
          case needle of
            (c:"") -> do
                cnt <- E.run $ EB.enumFile fname ==<< countCharBS c
                print cnt
            _ -> print "I need a single char to process."
      _ -> print "Error in accessing file."
```

Our Iteratee is dead-simple: We ask for the next line from the stream with
```head``` and if we haven't reached the end of the stream, we count the
occurrences of our char ```needle``` inside ```t```. As you can imagine,
```run``` actually runs our Iteratee, using ```==<<``` which is
"the most primitive stream operator. ```enum ==<< iter``` returns a
new iteratee which will read from ```enum``` before continuing."

So in this example, who is the ```Enumerator```? Well, unsurprisingly enough
is ```enumFile```, which open a file an returns an ```Enumerator``` that, as the
name implies, enumerates that file.

Bear in mind that we need to evaluate our loop strictly, or the memory
consumption of our program will explode. This is trivial if we recall how
Haskell manages lazyness: if the result is not demanded, it allocates a
```thunk```, which is an unevaluated computation. This can be problematic
for performance-critical code.

### Counting ```Char``` occurences in large files
The next step I wanted to achieve was to count occurrences of a certain ```Char```
inside a large file, to see how well Iteratee IO behaved. So I downloaded a random
dataset from the [1000 Genome Project](ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/data/HG00096/sequence_read/SRR062634_1.filt.fastq.gz)
to answer this question: How many [Adenine](http://en.wikipedia.org/wiki/Adenine)
(modeled inside the file with the letter ```A```) are present in this DNA Genome?
The compressed file was around ```2.2GB```, whereas the uncompressed version
took up to ```6GB``` of disk space.

### Normalizing with ```sed```
As you may image, a file of that type doesn't merely contain the sole list
of nucleobased which compose the whole DNA sequence, but they are interspersed
with metadata that is used to interpret these data in a meaningful way. For
the purpose of our research, the latter are simply noise we want to get rid of.
There are many ways to complete this task, but for such a big file I've found
convenient processing it with [sed](http://www.grymoire.com/Unix/Sed.html).
Like almost every Unix tools it's very efficient even for huge files, and 
the best thing is that require constant memory for the whole computation.
Let's take an excerpt of the unprocessed file with head:

```
@SRR062634.1 HWI-EAS110_103327062:6:1:1092:8469/1
GGGTTTTCCTGAAAAAGGGATTCAAGAAAGAAAACTTACATGAGGTGATTGTTTAATGTTGCTACCAAAGAAGAGAGAGTTACCT
+
@C'@9:BB:?DCCB5CC?5C=?5@CADC?BDB)B@?-A@=:=:@CC'C>5AA+*+2@@'-?>5-?C=@-??)'>>B?D@?*?A##
@SRR062634.2 HWI-EAS110_103327062:6:1:1107:21105/1
ACCGTGAGCAATCAGCTGCCATCAACGTGGAGGTAAGACTCTCCACCTGCAAAAACATTACAACTTGCTGAAGGCTGAGATACTT
+
FDEFF?DFEFE?BEEEEED=DB:DCEAEEB,CC=@B=5?B?CC5C?B+A??=>:CC<9-B2=@>-?:-<A@@A?9>*0<:'0%6,
@SRR062634.3 HWI-EAS110_103327062:6:1:1110:17198/1
TAGATATTTTTGTTTTAACTGCTGTAGAAAATTAAGACATAAACTAAGAAATATCCCATGAAGGAATGAGTATACTGTTTCTACT
+
-?3-C22646@-@3@@3-=-====CBB@DB-A-=-AA@C-<AA7>D=ABDA;?CDDDD5D?DD55:>:AB>5?-CCCC#######
@SRR062634.4 HWI-EAS110_103327062:6:1:1112:12923/1
AGATGAGTTTCACAAAGTAAGCAACTTGATATCCAAATAATCAACACCCAACTCAAGAAAAAGATCATTACCAGAAACTAATAAA
+
??EEEDB?D-?AAA-AA?>->BC:ADB:--A55ACCA:D6C:?5@CADD6=C5:CD?D4;,::?,CC-5A@C-?D5@+-BB?BC*
@SRR062634.5 HWI-EAS110_103327062:6:1:1113:19453/1
CTGTATCTAGGTTTTGTCCTTACATGTATATAACCTACACCCACAGTTTACCATCCGTGACATTTTCTTTCCTCTCCAGTCGTAC
+
CC?-?BAAB?E:B@@A7A?5CCBBBB@B?ABB?B@BB=B-BB=?#########################################
```

As you can see, we got a entire "good" line, as well as several "bad" lines
we would like to filter. With ```sed``` we can obtain a clean file in four
passes:

```
sed '/+/d' SRR062634_1.filt.fastq > DNA1.bin
sed '/^@/d' DNA1.bin > DNA2.bin
sed '/#$/d' DNA2.bin > DNA3.bin
sed '/^.*?.*$/d' DNA3.bin > DNA_final.bin
```

In a nutshell, these say to ```sed```:

* Remove all lines that match ```+```

* Remove all lines that start with ```@```

* Remove all lines that end with ```#```

* Remove every line which contains ```?```

Let's see the result of our ```DNA_final.bin```:

```
GGGTTTTCCTGAAAAAGGGATTCAAGAAAGAAAACTTACATGAGGTGATTGTTTAATGTTGCTACCAAAGAAGAGAGAGTTACCT
ACCGTGAGCAATCAGCTGCCATCAACGTGGAGGTAAGACTCTCCACCTGCAAAAACATTACAACTTGCTGAAGGCTGAGATACTT
TAGATATTTTTGTTTTAACTGCTGTAGAAAATTAAGACATAAACTAAGAAATATCCCATGAAGGAATGAGTATACTGTTTCTACT
AGATGAGTTTCACAAAGTAAGCAACTTGATATCCAAATAATCAACACCCAACTCAAGAAAAAGATCATTACCAGAAACTAATAAA
CTGTATCTAGGTTTTGTCCTTACATGTATATAACCTACACCCACAGTTTACCATCCGTGACATTTTCTTTCCTCTCCAGTCGTAC
TTAGGTTTTAATGTTTGGGAAACTAAATCTCTCCTCTATCCAAGTGTACATGCTTTCGTTTTATCCCTTTGTCTCAATCAATGAC
GATGTGCAGGTTTGTTACACAGGTAAATGTGTGCCATGGTGGTTTGCTGCACCTATCAACCCACCCCTTAGGTATCAAGATCTCC
TATAATTTACGAAAATTCTTTAATGCTGAAAAAAAGATGAAAATTCATTAATCTGAATGTCAATTCGGTCTTGTTCCAAACAGTT
GTGAAAGCTTAATGATACCATATGTGCTAAATTGTAAATTAATCTATCTCACCCGTTTCAGGAATTTCTTTTCAGTATTAATAGG
TTAGAATTTTATAATACAAAACATGCCTATGTAAATCACCGGAAAATTTTTCTAATGTCCCTATTCCATAAACATATTTATTTTA
GAATGGAATAGAATGGGATGGAATGAAACCGAATTGTCTGGAATGGAGTAGAATGGAATGCACATGAACTGACTGGTGTGGGATG
ATGGGGCCAGATTTTAAAAAGCCTTGTAGGCAAGGGGTAGGGATGTGGGGTTTGTTGTATGTTCTATGGGAGGAGTTGGGGGATT
AGAGATTGCATAACCGGATGCGTGAATTCAAAAAAGTTAAAATAACTTCATGTGATTTCGTTGTTGTTTTTTGTCATCCTGGTGG
AAAGGACATGAAAGAAAATAGCTAATTCCAGGTGGGCTCAGAACCTCCACTTTGTAACAGGGTATGGAGAGTGCAGCCCCACTGA
TTGTTGCCTTCAGCCTCTCTCATATCTAGTTCCGTATTCAGTACTAACTTTAACTCCCCTTAAAGTTATTACTCTAGCCACAATT
AGAGTATTTCCTTGCCTCCAGAGGTTTACACTAGCTCCCCAGCAATGGATTGAAATGTCTGAGATGACAGATATAGATAGAGTTC
GTTTATGACTGTATACTATTATAGACATTGCTACAAAGATGATCATAGCCATAACGGAAATAGTTTCTATAGAGGAAACAATGTT
TGTTACCTAAGGAGCAATAAACTGGTTGTTAGAAACACCAGACTTTGATAACACTGCATGACAGATCTCTGGCATCAAGGGGAAA
CTGTGTACTTTACTAATCTTTTTCTCCCTTTGGAATGATACTGCATCTACTCTTTTGCACCTTGCTATTGAATGGCTAGTCATAT
TGTACATAAAATATCAAAGTACCCAAATTATACATTATATACTGTACATAAATTATGAAATTATATCAAATATATTTTATATCAT
```

Ok! Now we have a clean file to test. As a nice side-effect, we end up with
a file ```50%``` smaller than the original.

### Finally answering the question
We can now run our iteratee computation on this huge file (```~3GB```):

```
./dna_counter DNA_final.bin "A"
Right 719528862
```

The computation is also pretty fast, allowing to answer our question in under
```20 seconds```. So we have an average of ```719``` millions Adenine huh?
Pretty impressive.

### Conclusions
Iteratee IO is an effective, fun and performant way to do IO, because it
allows us to see computations as streams and to reason in terms of stream
processing. I'm looking forward to experimenting further with it.

Oh, one last thing, though: Haskell rocks.
