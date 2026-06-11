{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad       (forM_, when)
import qualified Data.Text.IO        as T
import           System.Directory
  ( copyFile, createDirectoryIfMissing
  , doesDirectoryExist
  , getCurrentDirectory
  , listDirectory, removeDirectoryRecursive, removeFile
  )
import           System.Environment  (getArgs)
import           System.Exit        (ExitCode (..), exitWith)
import           System.FilePath    ((</>))
import           System.Process     (readProcess, system)

usage :: IO ()
usage = T.putStrLn "usage: site-ctl [build|watch|clean|publish]"

run :: String -> IO ()
run cmd = system cmd >>= exitWith

-- | Run a shell command, printing it first. Exit on failure.
runQuiet :: String -> IO ()
runQuiet cmd = do
  putStrLn $ "  + " ++ cmd
  ec <- system cmd
  case ec of
    ExitSuccess   -> return ()
    ExitFailure n -> do
      putStrLn $ "  ✗ command exited with code " ++ show n
      exitWith ec

-- | Recursively copy the contents of srcDir into dstDir.
--   Preserves directory structure, skips .git.
copyTree :: FilePath -> FilePath -> IO ()
copyTree srcDir dstDir = do
  createDirectoryIfMissing True dstDir
  entries <- listDirectory srcDir
  forM_ entries $ \name -> do
    let src = srcDir </> name
        dst = dstDir </> name
    isDir <- doesDirectoryExist src
    if isDir
      then unlessGit name $ copyTree src dst
      else unlessGit name $ copyFile src dst
  where
    unlessGit name act
      | name == ".git" = return ()
      | otherwise      = act

-- | Remove everything in a directory except .git.
cleanExceptGit :: FilePath -> IO ()
cleanExceptGit dir = do
  entries <- listDirectory dir
  forM_ entries $ \name -> do
    let path = dir </> name
    unlessGit name $ do
      isDir <- doesDirectoryExist path
      if isDir
        then removeDirectoryRecursive path
        else removeFile path
  where
    unlessGit name act
      | name == ".git" = return ()
      | otherwise      = act

publish :: IO ()
publish = do
  -- 1. Capture the current branch so we can return to it
  curBranch <- trim <$> readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
  putStrLn $ "Current branch: " ++ curBranch

  -- 2. Rebuild to make sure _site is fresh
  putStrLn "\n== Building site =="
  run "cabal run site -- build"

  -- 3. Stash any uncommitted changes (just in case)
  putStrLn "\n== Stashing uncommitted changes =="
  runQuiet "git stash --include-untracked --quiet 2>/dev/null || true"

  -- 4. Switch to gh-pages
  putStrLn "\n== Switching to gh-pages =="
  runQuiet "git checkout gh-pages"

  -- 5. Nuke everything except .git
  putStrLn "\n== Replacing gh-pages content with built site =="
  cwd <- getCurrentDirectory
  cleanExceptGit cwd

  -- 6. Copy _site/ contents into root
  let siteDir = cwd </> "_site"
  siteExists <- doesDirectoryExist siteDir
  when (not siteExists) $ do
    putStrLn "  ✗ _site directory not found — did the build succeed?"
    exitWith (ExitFailure 1)
  copyTree siteDir cwd

  -- 7. Commit and push
  putStrLn "\n== Committing and pushing =="
  runQuiet "git add -A"
  runQuiet "git commit -m \"publish: site rebuild\""
  runQuiet "git push origin gh-pages"

  -- 8. Switch back to the original branch
  putStrLn "\n== Switching back =="
  runQuiet $ "git checkout " ++ curBranch

  -- 9. Restore stashed changes
  runQuiet "git stash pop --quiet 2>/dev/null || true"

  putStrLn "\n✓ Published!"

  where
    trim = reverse . dropWhile (== '\n') . reverse . dropWhile (== '\r')

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["build"]   -> run "cabal run site -- build"
    ["watch"]   -> run "cabal run site -- watch"
    ["clean"]   -> run "cabal run site -- clean"
    ["publish"] -> publish
    _           -> usage >> exitWith (ExitFailure 64)