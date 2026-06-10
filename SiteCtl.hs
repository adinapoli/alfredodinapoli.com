{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO       as T
import           System.Environment (getArgs)
import           System.Exit        (ExitCode (..), exitWith)
import           System.Process     (system)

usage :: IO ()
usage = T.putStrLn "usage: site-ctl [build|watch|clean]"

run :: String -> IO ()
run cmd = system cmd >>= exitWith

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["build"] -> run "cabal run site -- build"
    ["watch"] -> run "cabal run site -- watch"
    ["clean"] -> run "cabal run site -- clean"
    _         -> usage >> exitWith (ExitFailure 64)
