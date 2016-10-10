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
was around, and sometimes is just super easy to slurp the output of a cli application and to decode that from JSON.
Short story short, this is why we didn't piggyback on this excellent third-party library, but that's a bit of an OT.
What's important is that to solve our problem, we only need two commands from the
`aws-cli`: [list-images](http://docs.aws.amazon.com/cli/latest/reference/ecr/list-images.html)
to retrieve all our images, and
[batch-delete-image](http://docs.aws.amazon.com/cli/latest/reference/ecr/batch-delete-image.html) to
delete the ones which meet our criteria (in our case that would be deleting anything older than 2 months).

`list-image` doesn't return the whole set of images, as this would be a beefy JSON! What it does instead,
is to return a JSON packet with a `Token` identifying if we have more data to fetch.
This is a standard `pagination` technique: other strategies would be, for example, to return the current page
and the total number of pages, so that the user can advance forward or backward.
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

A `ForwardPaginator` effectively models a tree of computations; we can have a _leaf_, meaning we have just
started our machine and are at step zero, or a _fetch_ step that, given an input `i` will produce a
triple `(newInput, output, paginator)`, doing (or not) some monadic effect in the process
(thus the `m` wrapping). Due to the fact we could have exhausted our input, we encapsulate this
possibility in a `Maybe`, which explains the presence of those `Maybe i`. We can even define what seems
to be a legal `Functor` instance for it:

``` haskell
instance Functor m => Functor (ForwardPaginator m i) where
  fmap f (PaginatorLeaf  a) = PaginatorLeaf (f a)
  fmap f (PaginatorFetch g) = PaginatorFetch $ \nextToken -> (\(x,y,z) -> (x, f y, fmap f z)) <$> g nextToken
```

(Note to the reader: I have the intuition we should be able to define a `Contravariant` instance for our
`ForwardPaginator`, but I'm not 100% sure as `i` appears both in positive and negative position. A `Profunctor`
even? Please comment below or on Reddit if you think this is possible, I simply haven't tried yet.)

If you squint hard, you will recognise that what we have in the `PaginatorFetch` step is essentially `Mealy` machine!
This is not very surprising;
[Neil Mitchell used it in Shake](http://neilmitchell.blogspot.it/2013/12/progress-reporting-in-shake.html)
only to discover his data structure was indeed a Mealy machine and his definition was almost verbatim to the
one included in the [machine package](http://hackage.haskell.org/package/machines-0.6.1/docs/Data-Machine-Mealy.html).
What I find cool is that both me and Neil went through the same creative process; we modeled our solution using an
abstraction we later found out be something already present in literature! I find both depressing and invigorating to
discover that your clever idea is something someone thought about a long time before you! Oh well, at least that
gave me the confidence I was on the right track. Incidentally,
[Ollie blogged](https://ocharles.org.uk/blog/posts/2013-08-01-getting-started-with-netwire-and-sdl.html)
in 2013 about FRP and Netwire, and guess what his `Auto` type looks like ;)

The reader might be thinking by now "Ok, but what can you do with this?" A Mealy machine is something very simple at
its heart, and copying its definition from Wikipedia _"...is a finite-state machine whose output values are
determined both by its current state and the current inputs.[...]"_. Simply put, we can use the current state and
the current input(s) to decide where to go next (which could be advance the machine or stop altogether). To be
completely honest with you, whilst writing this blog post, I was on the fence about considering what's inside a
`PaginatorFetch` a Mealy or a Moore machine, as it resemble a bit of both, but I _eventually settle on the former,
as effectively it's the input (the "token") which determines if we can step further or not_.

Armed with our `ForwardPaginator`, let's generalise it to our domain problem:

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
  initialState <- run "aws" cmd
  case JSON.eitherDecode (toS initialState) of
    Left ex -> yieldZero ex
    Right (ECRListImages Nothing items) -> return (Nothing, items, PaginatorLeaf mempty)
    Right (ECRListImages mbToken items) -> return (mbToken, items, fetch)
  where
    yieldZero ex = do
      echo "aws ecr list-images failed to decode to valid JSON. Error was: "
      echo (toS . show $ ex)
      return (Nothing, mempty, PaginatorLeaf mempty)
    cmd = [ "ecr"
          , "list-images"
          , "--region"
          , "eu-west-1"
          , "--repository-name"
          , repo
          ]
    fetch :: ECRPaginator Sh [ECRImage]
    fetch = PaginatorFetch $ \token -> case token of
      Nothing -> return (Nothing, mempty, PaginatorLeaf mempty)
      Just t  -> do
        rawJson <- run "aws" cmd
        case JSON.eitherDecode (toS rawJson) of
          Left ex -> yieldZero ex
          Right (ECRListImages mbToken items) -> return (mbToken, items, fetch)
```

The caveat here is that we need to repeat the call to `aws ecr` twice as we need to call it at least once to
acquire a valid token, so that externally we will be able to pass `Nothing` to our paginator to start it. I have
chosen `Sh` as my monad of choice (from the [shelly](http://hackage.haskell.org/package/shelly) package), so that
I can run `bash` commands easily.

Now the fun begins! What we can do with this paginator and more generally with a `ForwardPaginator`?

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
concatenate all the results together. This would also effectively allow us to return _all_ the `ECRImage`(s)
at once, but beware that this would load them into memory -- not recommended for your production services!

``` haskell
ecrListImages :: Repository -> IO [ECRImage]
ecrListImages repo = shelly $ foldPaginator (ecrListImagesPaginated repo) Nothing mempty
```

Something nice we can do with a `ForwardPaginator` is being able to find a particular element matching
a predicate, short-circuiting our paginator as soon as we find a match, in order to avoid work and more
generally expensive calls to external services:

``` haskell
findPaginator :: Monad m
              => ForwardPaginator m i [a]
              -> Maybe i
              -> (a -> Bool)
              -> m (Maybe a)
findPaginator (PaginatorLeaf v) _ prd = return $ List.find prd v
findPaginator (PaginatorFetch cont) tkn prd = do
  (t',items,cont') <- cont tkn
  case List.find prd items of
    Just i  -> return $ Just i
    Nothing -> findPaginator cont' t' prd
```

## Pure or impure? Pick your monad!

To wrap up this blog post, I also wanted to show you how we are not bounded to use a "impure" monad for
our `ForwardPaginator`: we could use something like `Identity`, `State`, `Reader` and so on and so forth. As
an example, we will create a `ForwardPaginator` which can be built out of a pure function (full disclosure: `fib`,
the classic, hehe) and everything will be pure to please the Haskell gods. Let's start by defining both our
pure function and the associated paginator:

``` haskell
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibPaginator :: ForwardPaginator Identity Int Integer
fibPaginator = PaginatorFetch $ \continue -> case continue of
  Nothing -> return (Just 1, fib 0, fibPaginator)
  Just i  -> return (Just $ i + 1, fib i, fibPaginator)
```

The slight twist is that in case we have no initial input, we return the base case of the recursion, otherwise
we iterate in an infinite fashion, exactly like the original `fib` function. Now we can easily step the paginator
using `next` to get one result at time, or create a convenient `take` function to get values out our infinite
stream:

``` haskell
takePaginator :: (Monad m) => ForwardPaginator m i a -> Maybe i -> Int -> m [a]
takePaginator (PaginatorLeaf v) _ _ = return [v]
takePaginator (PaginatorFetch cont) tkn n
  | n <= 0 = return []
  | otherwise = do
    (newToken, o, newPaginator) <- cont tkn
    (o :) <$> takePaginator newPaginator newToken (n - 1)
```

Using it is simple enough:

``` haskell
ghci> runIdentity $ takePaginator fibPaginator Nothing 10
[0,1,1,2,3,5,8,13,21,34]
```

As a bonus, as `ForwardPaginator` is a functor, we can easily map a function on the output values as we stream them:

``` haskell
ghci> runIdentity $ takePaginator ((*2) <$> fibPaginator) Nothing 10
[0,2,2,4,6,10,16,26,42,68]
```

## Stepping backwards

A bit of a pet peeve the reader migth have with this paginator is that is lacks the ability to step
backward, and that would certainly be a valid concern. I still think though that adding the ability to
iterate backward should be possible provided that we create a function `back` which bound the paginator
monad to be a `MonadState (Maybe i)`, so that we can store the previous token and go backward and forward
as we please. Something like this, for example:

``` haskell
prev :: MonadState (Maybe i) m
     => ForwardPaginator m i a
     -> Maybe i
     -- ^ The initial input state to use.
     -> m (Maybe i, Maybe a, ForwardPaginator m i a)
```

I think we need to yield a `Maybe a` as output in case we want to step backward but we are already at the first
"page": in that case, we should yield no result. Maybe, if the readers are interested, I could explore this
possibility in a subsequent blog post, which should effectively give us a `Paginator` worth its name, to be
used in each scenario which requires bidirectional pagination.

## Conclusions

The ideas presented here are very simple but at the same time quite effective; They allowed me to solve my
original problem in a nice compact way. Using `findPaginator` and the `Functor` instance I was able to first
stop as soon as the current result set contained values I was interested in, and I was able to "zoom" only on
pieces of the `ECRImage` data structure to extract things like the `ImageDigest`. So, next time you need to
implement some form of pagination, remember you have the arsenal of Mealy and Moore Machines at your disposal:
it's not surprising they are called _stream transducers_!

