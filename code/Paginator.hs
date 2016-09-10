{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Paginator where

import           Control.Monad.State.Strict
import qualified Data.Aeson as JSON
import           Data.Aeson.TH
import           Data.Functor.Identity
import qualified Data.List as List
import           Data.String.Conv
import qualified Data.Text as T
import           Shelly

--------------------------------------------------------------------------------
data ECRImage = ECRImage {
    imageDigest :: T.Text
  , imageTag    :: Maybe T.Text
  } deriving (Show, Eq)

deriveFromJSON defaultOptions { omitNothingFields = True } ''ECRImage

--------------------------------------------------------------------------------
type NextToken = T.Text

type Repository = T.Text

--------------------------------------------------------------------------------
data ECRListImages = ECRListImages {
    nextToken :: Maybe NextToken
  , imageIds  :: [ECRImage]
  }

deriveFromJSON defaultOptions { omitNothingFields = True } ''ECRListImages

--------------------------------------------------------------------------------
data ForwardPaginator m i o =
    PaginatorLeaf o
  | PaginatorFetch (Maybe i -> m (Maybe i, o, ForwardPaginator m i o))

instance (Show i, Show o) => Show (ForwardPaginator m i o) where
  show (PaginatorLeaf o) = show o
  show (PaginatorFetch cont) = "PaginatorFetch<continuation>"

----------------------------------------------------------------------------------
type ECRPaginator m a = ForwardPaginator m NextToken a

-- NOTE: Valid Functor instance?
instance Functor m => Functor (ForwardPaginator m i) where
  fmap f (PaginatorLeaf  a) = PaginatorLeaf (f a)
  fmap f (PaginatorFetch g) = PaginatorFetch $ \nextToken -> (\(x,y,z) -> (x, f y, fmap f z)) <$> g nextToken

----------------------------------------------------------------------------------
foldPaginator :: (Monad m, Monoid a) => ForwardPaginator m i a -> Maybe i -> a -> m a
foldPaginator (PaginatorLeaf items) _ acc = return (items `mappend` acc)
foldPaginator (PaginatorFetch cont) tkn acc = do
  (t', acc', res) <- cont tkn
  case res of
    leaf@(PaginatorLeaf _) -> foldPaginator leaf Nothing acc
    nextFetch              -> foldPaginator nextFetch t' acc'

--------------------------------------------------------------------------------
next :: Monad m
      => ForwardPaginator m i a
      -> Maybe i
      -- ^ The initial input state to use.
      -> m (Maybe i, a, ForwardPaginator m i a)
next (PaginatorLeaf i) _ = return (Nothing, i, PaginatorLeaf i)
next (PaginatorFetch cont) tkn = cont tkn

----------------------------------------------------------------------------------
--prev :: MonadState (Maybe i) m
--     => ForwardPaginator m i a
--     -> Maybe i
--     -- ^ The initial input state to use.
--     -> m (Maybe i, a, ForwardPaginator m i a)
--prev (PaginatorLeaf i) _ = return (Nothing, i, PaginatorLeaf i)
--prev (PaginatorFetch cont) tkn = cont tkn

----------------------------------------------------------------------------------
takePaginator :: (Monad m) => ForwardPaginator m i a -> Maybe i -> Int -> m [a]
takePaginator (PaginatorLeaf v) _ _ = return [v]
takePaginator (PaginatorFetch cont) tkn n
  | n <= 0 = return []
  | otherwise = do
    (newToken, o, newPaginator) <- cont tkn
    (o :) <$> takePaginator newPaginator newToken (n - 1)

--------------------------------------------------------------------------------
findPaginator :: Monad m => ForwardPaginator m i [a] -> Maybe i -> (a -> Bool) -> m (Maybe a)
findPaginator (PaginatorLeaf v) _ prd = return $ List.find prd v
findPaginator (PaginatorFetch cont) tkn prd = do
  (t',items,cont') <- cont tkn
  case List.find prd items of
    Just i  -> return $ Just i
    Nothing -> findPaginator cont' t' prd

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
ecrListImages :: Repository -> IO [ECRImage]
ecrListImages tgt = do
  let pag = ecrListImagesPaginated tgt
  shelly $ silently $ foldPaginator pag Nothing mempty


--
-- Fibo paginator
--

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibPaginator :: ForwardPaginator Identity Int Integer
fibPaginator = PaginatorFetch $ \continue -> case continue of
  Nothing -> return (Just 1, fib 0, fibPaginator)
  Just i  -> return (Just $ i + 1, fib i, fibPaginator)
