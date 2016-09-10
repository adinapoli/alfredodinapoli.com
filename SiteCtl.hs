{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import qualified Data.Text as T
import Shelly
import Text.Printf
import Data.Monoid


----------------------------------------------------------------------
data DeployTarget = GoDaddy deriving (Show, Eq, Read)


----------------------------------------------------------------------
toIP :: DeployTarget -> T.Text
toIP GoDaddy = T.pack "188.121.46.128"

----------------------------------------------------------------------
toDeployPath :: DeployTarget -> T.Text
toDeployPath GoDaddy = "adinapoli@"
                     <> toIP GoDaddy
                     <> ":/home/content/24/10017624/html/"


----------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--publish"] -> publish
    _ -> putStrLn "usage: site-ctl --publish [comment]"


publish :: IO ()
publish = shelly $ verbosely $ escaping False $ do
  updateSite
  syncWithRsync


syncWithRsync :: Sh ()
syncWithRsync = do
  run_ "rsync" (T.words $ "-avzr -e ssh --rsync-path=bin/rsync _site/* " <>
                toDeployPath GoDaddy)

updateSite :: Sh ()
updateSite = run_ "site" ["build"]
