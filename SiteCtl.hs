{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import qualified Data.Text as T
import Shelly
import Text.Printf
import Data.Monoid


----------------------------------------------------------------------
data DeployTarget = GoDaddy | DigitalOcean deriving (Show, Eq, Read)


----------------------------------------------------------------------
toIP :: DeployTarget -> T.Text
toIP DigitalOcean = T.pack "5.101.97.178"
toIP GoDaddy = T.pack "188.121.46.128"

----------------------------------------------------------------------
toDeployPath :: DeployTarget -> T.Text
toDeployPath DigitalOcean = "root@"
                          <> toIP DigitalOcean
                          <> ":/home/purely-functional-io/html"
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
  run_ "rsync" (T.words $ "-avzr -e ssh _site/* " <>
                toDeployPath DigitalOcean)

updateSite :: Sh ()
updateSite = run_ "site" ["build"]

commit :: String -> Sh ()
commit comment = escaping False $ do
  run_ "git" (T.words . T.pack $ printf "add . && git commit -m \"%s\"" comment)


push :: String -> Sh ()
push branch = run_ "git" (T.words $ T.pack (printf "push origin %s" branch))
