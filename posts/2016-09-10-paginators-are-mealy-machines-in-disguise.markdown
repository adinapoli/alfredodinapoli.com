---
title: Paginators are Mealy Machines in disguise
tags: haskell, fp
---

# Paginators are Mealy Machines in disguise

```
Summary: At work I needed to stream some data out from a service which returned data in paginated chunks. 
Using a very simple data type based on Mealy Machines worked surprisingly well.
```

One of the aspect I enjoy most of programming is when you have the chance of
applying something you have learned in the real world. A couple of weeks ago
I needed to create a tool to "garbage collect" old [ECR](https://aws.amazon.com/ecr/) images.
Very simply put, _ECR_ stands for _EC2 Container Registry_ and is no more, no less,
a private Docker Registry you can use as part of the impressive AWS (_Amazon Web Service_) toolkit.

_ECR_ works by having a set of _repositories_, and for each of them you can upload up to 500 Docker images.
If you exceed this limit, you will have either to delete some images to create free space, or contact
the Amazon customer service to dump the upper limit up. My team uses `ECR` to store the images associated to
the Haskell micro services we deploy using [Elastic Beanstalk](https://aws.amazon.com/documentation/elastic-beanstalk/),
and each time we do a deploy, we create and upload a new versioned image for each micro service, so it comes as no
surprise that space in the repositories is going to finish sooner or later. So the problem is simple: "I have some
images stored in the cloud and I need to delete them".

Now, we could have used the quite impressive [amazonka-ecr](http://hackage.haskell.org/package/amazonka-ecr) to solve
our problem, but historically we have been using the [aws-cli](https://aws.amazon.com/cli/) even before Amazonka
was around, and sometimes is just super easy to slurp the output of a cli application and to encode that as JSON.
Short story short, this is why we didn't piggyback on this excellent third-party library, but that's a bit of an OT.
What's important is that to solve our problem, we really need two commands from the
`aws-cli`: [list-images](http://docs.aws.amazon.com/cli/latest/reference/ecr/list-images.html)
to retrieve all our images, and [batch-delete-image](http://docs.aws.amazon.com/cli/latest/reference/ecr/batch-delete-image.html) to
delete the ones which meet our criteria (in our case that would be deleting anything older than 2 months).

`list-image` doesn't return the whole set of images, as this would be a beefy JSON! What it does instead, is to return a JSON
packet with a `Token` identifying if we have more data to fetch. This is a standard `pagination` technique: other strategies
would be, for example, to return the current page and the total number of pages, so that the user can advance forward or backward. 
We can easily model an `ECRImage` as a simple data type by following the specification on the `list-image` page:

``` haskell

-- Useful import to use in the rest of the post
import qualified Data.Aeson as JSON
import           Data.Aeson.TH
import           Data.Functor.Identity
import qualified Data.List as List
import           Data.String.Conv
import qualified Data.Text as T
import           Shelly


data ECRImage = ECRImage {
    imageDigest :: T.Text
  , imageTag    :: Maybe T.Text
  } deriving (Show, Eq)

deriveFromJSON defaultOptions { omitNothingFields = True } ''ECRImage

type NextToken = T.Text

data ECRListImages = ECRListImages {
    nextToken :: Maybe NextToken
  , imageIds  :: [ECRImage]
  }

deriveFromJSON defaultOptions { omitNothingFields = True } ''ECRListImages
```

The `ECRListImages` is an umbrella type we define to parse the raw JSON that AWS gives us, which will include the
token _AND_ the data fetched so far. When I approached this problem, I knew two things for sure:

* I didn't want to fetch the whole dataset into memory, but rather processing it in chunks
* The problem itself was screaming "streaming!"

Although I could have simply written a recursive function which would fetch the current data and the token,
process it, and recur in case we still had data to fetch, that stroke me as a poor solution. Not bacause it
was _intrinsically_ bad, but only because it felt a bit ad-hoc and didn't compose very well. What if I wanted
to step through the data "one chunk" at the time? What if I wanted to filter each chunk according to a predicate
and retain only a subset of it? Sure, I could extend my function which a predicate to filter on, but that felt
even more ad-hoc. So I took a step backward and wondered if I could come up with a super tiny abstraction to
"step" through the data whilst retaining code reuse and composition. After some failing attempt, I came up with
this small data structure, which I'm calling here `ForwardPaginator` to stress the fact we cannot iterate
backward (yet), which is something I didn't need to support anyway:

``` haskell
data ForwardPaginator m i o =
    PaginatorLeaf o
  | PaginatorFetch (Maybe i -> m (Maybe i, o, ForwardPaginator m i o))
```

A `ForwardPaginator` effectively models a tree of computations; we can have a _leaf_, meaning we have exhausted
our result pool, or a _fetch_ step that, given an input `i` will produce a triple `(newInput, output, paginator)`,
doing (or not) some monadic effect in the process (thus the `m` wrapping). Due to the fact we could have exhausted
our input, we encapsulate this possibility in a `Maybe`, which explains the presence of those `Maybe i`. We can even
define what seems to be a legal `Functor` instance for it:

``` haskell
instance Functor m => Functor (ForwardPaginator m i) where
  fmap f (PaginatorLeaf  a) = PaginatorLeaf (f a)
  fmap f (PaginatorFetch g) = PaginatorFetch $ \nextToken -> (\(x,y,z) -> (x, f y, fmap f z)) <$> g nextToken
```

If you squint hard, you will recognise that what we have in the `PaginatorFetch` step is essentially `Mealy` machine! This is
not very surprising; [Neil Mitchell used it in Shake](http://neilmitchell.blogspot.it/2013/12/progress-reporting-in-shake.html)
only to discover his data structure was indeed a Mealy machine and his definition was almost verbatim to the
one included in the [machine package](http://hackage.haskell.org/package/machines-0.6.1/docs/Data-Machine-Mealy.html).
What I find cool is that both me and Neil went through the same creative process; we modeled our solution using an
abstraction we later found out be something already present in literature! I find both depressing and invigorating to
discover that your clever idea is something someone thought about a long time before you! Oh well, at least that gave me
the confidence I was on the right track. Incidentally, [Ollie blogged](https://ocharles.org.uk/blog/posts/2013-08-01-getting-started-with-netwire-and-sdl.html) in 2013 about FRP and Netwire, and guess what his `Auto` type looks like ;)

The reader might be thinking by now "Ok, but what can you do with this?" A Mealy machine is something very simple at
its heart, and copying its definition from Wikipedia _"...is a finite-state machine whose output values are
determined both by its current state and the current inputs.[...]"_. Simply put, we can use the current state and
the current input(s) to decide where to go next (which could be advance the machine or stop altogether). Armed with our
`ForwardPaginator`, let's generalise it to our domain problem:

``` haskell
type NextToken = T.Text
type Repository = T.Text -- Will use this later
type ECRPaginator m a = ForwardPaginator m NextToken a
```

Now, let's get the elephant out of the room and let me give you the (rather) uninteresting definition
of our ECR paginator. I personally think that the semantic of the data structure and the operations we can
perform of it are much more interesting, but I wanted to post a "real world" paginator just to prove this stuff
can also pay the bills ;)

``` haskell
ecrListImagesPaginated :: Repository -> ECRPaginator Sh [ECRImage]
ecrListImagesPaginated repo = PaginatorFetch $ \_ -> do
  initialState <- run "aws" [ "ecr"
                            , "list-images"
                            , "--region"
                            , "eu-west-1"
                            , "--repository-name"
                            , repo
                            ]
  case JSON.eitherDecode (toS initialState) of
    Left ex -> do
      echo "aws ecr list-images failed to decode to valid JSON. Error was: "
      echo (toS . show $ ex)
      return (Nothing, mempty, PaginatorLeaf mempty)
    Right (ECRListImages Nothing items) -> return (Nothing, items, PaginatorLeaf mempty)
    Right (ECRListImages mbToken items) -> return (mbToken, items, fetch)
  where
    fetch :: ECRPaginator Sh [ECRImage]
    fetch = PaginatorFetch $ \token -> do
      case token of
        Nothing -> return (Nothing, mempty, PaginatorLeaf mempty)
        Just t  -> do
          rawJson <- run "aws" [ "ecr"
                               , "list-images"
                               , "--next-token", t
                               , "--region"
                               , "eu-west-1"
                               , "--repository-name"
                               , repo
                               ]
          case JSON.eitherDecode (toS rawJson) of
            Left ex -> do
              echo "aws ecr list-images failed to decode to valid JSON. Error was: "
              echo (toS . show $ ex)
              return (Nothing, mempty, PaginatorLeaf mempty)
            Right (ECRListImages mbToken items) -> return (mbToken, items, fetch)
```

The caveat here is that we need to repeat the call to `aws ecr` twice as we need to call it at least once to
acquire a valid token, so that externally we will be able to pass `Nothing` to our paginator to start it. Now the
fun begins! What we can do with this paginator and more generally with a `ForwardPaginator`?

The first operation we can think of is effectively "stepping" the paginator, and implementing this function is not
very hard:

``` haskell
next :: Monad m
     => ForwardPaginator m i a
     -> Maybe i
     -- ^ The initial input state to use.
     -> m (Maybe i, a, ForwardPaginator m i a)
next (PaginatorLeaf i) _ = return (Nothing, i, PaginatorLeaf i)
next (PaginatorFetch cont) tkn = cont tkn
```

Note how this function is completely generic in terms of `m`, `i` and `a`, apart from the `Monad` constraint,
which means I can "step" arbitrary paginators -- talk about code reuse! Another thing we might want is to be
evil and fold all the data returned from the paginator into a giant collection. Not hard as well:

``` haskell
foldPaginator :: (Monad m, Monoid a) => ForwardPaginator m i a -> Maybe i -> a -> m a
foldPaginator (PaginatorLeaf items) _ acc = return (items `mappend` acc)
foldPaginator (PaginatorFetch cont) tkn acc = do
  (t', acc', res) <- cont tkn
  case res of
    leaf@(PaginatorLeaf _) -> foldPaginator leaf Nothing acc
    nextFetch              -> foldPaginator nextFetch t' acc'
```

Again, the only constrain is that our accumulator must be a `Monoid`, so that we can effectively
concatenate all the results together.


* Show `fibPaginator`
* Show `takePaginator`
* Real world: ECR paginator
* Introduce `foldPaginator`

* http://neilmitchell.blogspot.it/2013/12/progress-reporting-in-shake.html
* http://hackage.haskell.org/package/machines-0.6.1/docs/Data-Machine-Mealy.html
