---
title: Multiple concurrent file readers in Haskell
description: How we can have multiple concurrent readers on a file in Haskell
tags: haskell
---

_This is a Literate Haskell post. You can download and play with it in the
editor._


> {-# LANGUAGE BangPatterns        #-}
> {-# LANGUAGE RecordWildCards     #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TupleSections       #-}
> 
> module Main (
>       HandleManager
>     , newEmptyHandleManager
>     , closeHandleManager
>     , ReadHandle
>     , WriteHandle
>     , openDBFile
>     , closeDBFile
>     , openReadHandle
>     , closeWriteHandle
>     , writeToHandle
>     , readFromHandle
>     , readHandleAbsoluteSeek
>     -- * Testing only
>     , demo
>     ) where
> 
> import           Control.Exception
> import           Control.Monad
> import           Control.Monad.Except
> import           Control.Monad.Reader
> 
> import           Data.ByteString (ByteString)
> import           Data.ByteString.Builder (Builder)
> import qualified Data.ByteString.Builder as BL
> import qualified Data.ByteString.Char8 as C8
> import           Data.Map (Map)
> import qualified Data.Map as Map
> import           Data.Word
> import           System.Random
> 
> import           GHC.Stack
> 
> import           Ouroboros.Network.MonadClass
> import           Ouroboros.Storage.Immutable.FS
> 
> --
> -- crucial question: is complexity in this layer (by having lifetime maangement
> -- our side) worth it? if not, how do we stop readers from blocking slot
> -- writing?
> --
> 
> data HandleState m = HS {
>       hsReadHandles  :: !(Map FsPath [ReadHandle m])
>     , hsWriteHandles :: !(Map FsPath (WriteHandle m))
>     }
> 
> -- | An opaque reference to a 'FsHandle' \"manager\". Such manager is
> -- responsible for the lifetime management of the underlying handles, and
> -- most importantly is capable of \"suspending\" and resuming them in a way
> -- that allows concurrent reads and writes to happen on the same shared
> -- resource.
> data HandleManager m = HandleManager {
>       im :: !(TMVar m (HandleState m))
>     }
> 
> -- | Creates a new, empty 'HandleManager'.
> newEmptyHandleManager :: MonadSTM m => m (HandleManager m)
> newEmptyHandleManager =
>     HandleManager <$> atomically (newTMVar (HS mempty mempty))
> 
> -- | Closes the 'HandleManager', closing all the associated handlers it
> -- manages. You want to typically call this function only once at the end
> -- of your program.
> closeHandleManager :: HasFS m => HandleManager m -> m ()
> closeHandleManager HandleManager{..} = do
>     hs@HS{..} <- atomically $ takeTMVar im
>     let allReaders = Map.toList hsReadHandles
>     let allWriters = Map.toList hsWriteHandles
> 
>     forM_ allReaders $ \(_, rws) -> forM_ rws $ \(RH _ r) -> closeHandleRef r
>     forM_ allWriters $ \(_, (WH _ w)) -> closeHandleRef w
> 
>     atomically $ putTMVar im (hs { hsWriteHandles = mempty
>                                  , hsReadHandles  = mempty
>                              })
> 
> -- | Opaque references to read & write handles.
> --
> -- could extend this so that all readers of the same epoch share a reference
> -- an an index, so that as soon as one reader reads the entire index of the
> -- epoch, all readers immediately have access to it.
> -- we can decide when to read the index (or even allow the user to indicate
> -- whether it would be useful)
> data ReadHandle m =
>     RH FsPath (TMVar m (Either (FsHandle m, Word64) Word64))
> 
> 
> data WriteHandle m =
>     WH FsPath (TMVar m (Either (FsHandle m, Word64) Word64))
> 
> 
> -- | Opens a DB file, returning the (one and only) 'WriteHandle'. This is
> -- by design; this is the /only/ way to acquire a 'WriteHandle'. When we close
> -- the relevant file in 'closeDbFile', we also close any associated 'WriteHandle'.
> openDBFile :: HasFS m => HandleManager m -> FsPath -> m (WriteHandle m)
> openDBFile HandleManager{..} fp = atomically $ do
>     hs@HS{..} <- takeTMVar im
>     wh <- case Map.lookup fp hsWriteHandles of
>       Just _  -> error "you cannot open more than 1 writehandle per file."
>       Nothing -> do
>         WH fp <$> newTMVar (Right 0)
>     putTMVar im (hs { hsWriteHandles = Map.insert fp wh hsWriteHandles })
>     return wh
> 
> -- | Closes the DB file.
> -- NOTE(adn): the choice here is to abruptly close the write handle or to wait
> -- for the write to finish, which might delay closing the DB.
> closeDBFile :: forall m. (HasCallStack, HasFS m)
>             => HandleManager m
>             -> FsPath
>             -> m (Either FsError ())
> closeDBFile HandleManager{..} fp = do
>     hs@HS{..} <- atomically $ takeTMVar im
>     r <- runExceptT $ do
>         case Map.lookup fp hsReadHandles of
>              Nothing  -> return ()
>              Just rhs -> forM_ rhs $ \(RH _ mv) -> ExceptT $ closeHandleRef mv
>         case Map.lookup fp hsWriteHandles of
>              Nothing        -> return ()
>              Just (WH _ mv) -> ExceptT $ closeHandleRef mv
>     atomically (putTMVar im hs)
>     return r
> 
> -- | Internal utility function to close a handle.
> closeHandleRef :: (HasCallStack, HasFS m)
>                => TMVar m (Either (FsHandle m, Word64) Word64)
>                -> m (Either FsError ())
> closeHandleRef mv = do
>     wh <- atomically $ takeTMVar mv
>     r <- runExceptT $ case wh of
>       Right _     -> return ()
>       Left  (h,_) -> ExceptT $ hClose h
>     atomically (putTMVar mv wh)
>     return r
> 
> openReadHandle :: forall m. HasFS m
>                => HandleManager m
>                -> FsPath
>                -> m (ReadHandle m)
> openReadHandle HandleManager{..} fp = atomically $ do
>     hs@HS{..} <- takeTMVar im
>     rh <- RH fp <$> newTMVar (Right 0)
>     let aux :: Maybe [ReadHandle m] -> Maybe [ReadHandle m]
>         aux Nothing    = Just [rh]
>         aux (Just dhs) = Just (rh : dhs)
>     putTMVar im (hs { hsReadHandles = Map.alter aux fp hsReadHandles })
>     return rh
> 
> -- | Closes a previously-opened 'WriteHandle'.
> -- NOTE(adn): Here we are suppressing any internal error coming from closing
> -- the handles, but in future we might rethink this in case we do want to
> -- gracefully handle those failures.
> closeWriteHandle :: HasFS m => HandleManager m -> WriteHandle m -> m ()
> closeWriteHandle HandleManager{..} (WH fp _) = do
>     hs@HS{..} <- atomically $ takeTMVar im
>     hs' <- case Map.lookup fp hsWriteHandles of
>                 Nothing -> return hs
>                 Just (WH f mv) -> assert (f == fp) $ do
>                     _ <- void <$> closeHandleRef mv
>                     return (hs { hsWriteHandles = Map.delete fp hsWriteHandles })
>     atomically $ putTMVar im hs'
> 
> -- | Internal function, used when we need to write to a file
> -- NOTE(adn): Here we are suppressing any internal error coming from closing
> -- the handles, but in future we might rethink this in case we do want to
> -- gracefully handle those failures.
> withClosedReadHandles :: HasFS m
>                       => HandleManager m
>                       -> FsPath
>                       -> m a
>                       -> m a
> withClosedReadHandles HandleManager{..} fp action = do
>     hs@HS{..} <- atomically $ takeTMVar im
>     case Map.lookup fp hsReadHandles of
>          Nothing -> atomically (putTMVar im hs) >> action
>          Just dhs -> do
>              forM_ dhs $ \(RH fp1 mv) -> assert (fp == fp1) $ do
>                  -- This modifyMVar might temporarily block, but /we/ control how long that
>                  -- can be (in 'readFromHandle')
>                  rh <- atomically $ takeTMVar mv
>                  rh' <- case rh of
>                    Right i -> return $ Right i
>                    Left  (h,i)       -> do
>                      _ <- hClose h
>                      return $ Right i
>                  atomically $ putTMVar mv rh'
>              atomically (putTMVar im hs) >> action
> 
> -- | Seek on a 'ReadHandle', to support random-access reads.
> readHandleAbsoluteSeek :: (HasFS m, HasCallStack)
>                        => ReadHandle m
>                        -> Word64
>                        -> m (Either FsError ())
> readHandleAbsoluteSeek (RH _ mv) !n = runExceptT $ do
>     rh <- atomically $ takeTMVar mv
>     rh' <- case rh of
>              Left (h,_)  -> do
>                  ExceptT $ hSeek h AbsoluteSeek n
>                  return $ Left (h, n)
>              Right _ -> return (Right n)
>     atomically $ putTMVar mv rh'
> 
> -- | Low-level (but public) streaming read API.
> readFromHandle :: HasFS m
>                => HandleManager m
>                -> ReadHandle m
>                -> Int
>                -> m (Either FsError ByteString)
> readFromHandle HandleManager{..} (RH fp mv1) !n = do
>     hs@HS{..} <- atomically $ takeTMVar im
>     bytes <- case Map.lookup fp hsWriteHandles of
>         Nothing -> runExceptT $ do
>             rh <- atomically $ takeTMVar mv1
>             (rh', bytes) <- case rh of
>               Left p  -> uncurry readNext p
>               Right !i-> openAndReadNext i
>             atomically $ putTMVar mv1 rh'
>             return bytes
>         Just (WH _ mv2) -> runExceptT $ do
>             (rh, wh) <- atomically $ do
>                 readLock  <- takeTMVar mv1
>                 writeLock <- takeTMVar mv2
>                 return (readLock, writeLock)
>             (wh', (rh', bytes)) <- case rh of
>               Left p  -> (,) <$>               pure wh <*> uncurry readNext p
>               Right i -> (,) <$> suspendWriteHandle wh <*> openAndReadNext i
>             atomically $ do
>                 putTMVar mv1 rh'
>                 putTMVar mv2 wh'
>             return bytes
>     atomically $ putTMVar im hs
>     return bytes
>     where
>         openAndReadNext :: HasFS m
>                         => Word64
>                         -> ExceptT FsError m (Either (FsHandle m, Word64) Word64, ByteString)
>         openAndReadNext i = do
>            rh' <- ExceptT $ hOpen fp ReadMode
>            ExceptT $ hSeek rh' AbsoluteSeek i
>            readNext rh' i
> 
>         readNext :: HasFS m
>                  => FsHandle m
>                  -> Word64
>                  -> ExceptT FsError m (Either (FsHandle m, Word64) Word64, ByteString)
>         readNext h acc = do
>             bytes <- ExceptT $ hGet h n
>             return (Left (h, (acc + toEnum n)), bytes)
> 
>         suspendWriteHandle :: HasFS m
>                            => Either (FsHandle m, Word64) Word64
>                            -> ExceptT FsError m (Either (FsHandle m, Word64) Word64)
>         suspendWriteHandle (Right r)    = return (Right r)
>         suspendWriteHandle (Left (h,i)) = do
>             ExceptT $ hClose h
>             return  $ Right i
> 
> 
> -- | Low-level (but public) streaming write API
> writeToHandle :: HasFS m
>               => HandleManager m
>               -> WriteHandle m
>               -> Builder
>               -> m (Either FsError Word64)
> writeToHandle im (WH fp mv) bs = do
>     withClosedReadHandles im fp $ runExceptT $ do
>         wh <- atomically $ takeTMVar mv
>         (wh', bytesWritten) <- case wh of
>           Left (h,!acc) -> do
>               bytesWritten <- ExceptT $ hPut h bs
>               return (Left (h, acc + bytesWritten), bytesWritten)
>           Right !i -> do
>             h <- ExceptT $ hOpen fp ReadWriteMode
>             ExceptT $ hSeek h AbsoluteSeek i
>             bytesWritten <- ExceptT $ hPut h bs
>             return (Left (h, i + bytesWritten), bytesWritten)
>         atomically $ putTMVar mv wh'
>         return bytesWritten
> 
> -- We spawn 10 readers and 1 writer and we see what happens.
> demo :: IO (Either FsError ())
> demo = flip runReaderT "/var/tmp/" $ runExceptT $ do
>     loggerLock <- atomically $ newTMVar ()
>     -- Preparation
>     ExceptT $ withFile ["test.txt"] WriteMode $ \hdl -> do
>         forM_ [1..255] $
>           \(_ :: Int) -> hPut hdl (BL.intDec 9)
>         return $ Right ()
>     lift . lift $ threadDelay 1000
>     im <- ExceptT $ Right <$> newEmptyHandleManager
>     wh <- ExceptT $ Right <$> openDBFile im ["test.txt"]
>     forM_ [1..10] $ \rId -> fork $ do
>         h1  <- ExceptT $ Right <$> openReadHandle im ["test.txt"]
>         forM_ [1 .. 50] $ \(_ :: Int) -> do
>             x1  <- ExceptT $ readFromHandle im h1 1
>             ()  <- atomically $ takeTMVar loggerLock
>             lift . lift $
>                 say $ "R(" <> show (rId :: Int) <> ") = " <> C8.unpack x1
>             atomically $ putTMVar loggerLock ()
>             lift . lift $ do
>                 delay <- randomRIO (100 :: Int, 1000)
>                 threadDelay delay
>     fork $ do
>         forM_ [1 .. 10] $ \i -> do
>             bWritten <- ExceptT $ writeToHandle im wh (BL.intDec i)
>             ()  <- atomically $ takeTMVar loggerLock
>             lift . lift $ say $ "W(" <> show i <> ") = " <> show bWritten
>             atomically $ putTMVar loggerLock ()
>             lift . lift $ do
>                 delay <- randomRIO (100, 1000)
>                 threadDelay delay
>     lift . lift $ do
>         threadDelay (8 * 1000000)
>         say "Closing the DB.."
>     ExceptT $ closeDBFile im ["test.txt"]
>     ExceptT $ Right <$> closeHandleManager im
>     lift . lift $ say "Done"
