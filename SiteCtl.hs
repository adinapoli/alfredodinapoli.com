{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import qualified Data.Text as T
import Shelly
import Text.Printf
import Data.Monoid


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--publish", comment] -> publish comment
    ["--publish"] -> publish "Site update"
    _ -> putStrLn "usage: site-ctl --publish [comment]"


publish :: String -> IO ()
publish comment = shelly $ verbosely $ do
  updateSite
  commit comment
  push "master"
  syncWithRsync


syncWithRsync :: Sh ()
syncWithRsync = do
  run_ "rsync" (T.words "-r _site/* ~/github/adinapoli.bitbucket.org/")
  run_ "rsync" (T.words $ "-avzr -e ssh --rsync-path=bin/rsync _site/* " <>
                "adinapoli@188.121.46.128:/home/content/24/10017624/html/")

updateSite :: Sh ()
updateSite = run_ "site" ["build"]

commit :: String -> Sh ()
commit comment = escaping False $ do
  run_ "git" (T.words . T.pack $ printf "add . && git commit -m \"%s\"" comment)


push :: String -> Sh ()
push branch = run_ "git" (T.words $ T.pack (printf "push origin %s" branch))
