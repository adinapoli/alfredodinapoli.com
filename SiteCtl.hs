{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad       (forM_, void)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Shelly
import           System.Environment  (getArgs)
import           System.Exit         (ExitCode (..), exitWith)

usage :: IO ()
usage = T.putStrLn "usage: site-ctl [build|watch|clean|publish]"

-- | Remove all entries in a directory except .git.
cleanExceptGit :: FilePath -> Sh ()
cleanExceptGit dir = do
  entries <- ls dir
  forM_ entries $ \entry ->
    unless (toTextIgnore entry == ".git" || T.isSuffixOf "/.git" (toTextIgnore entry)) $
      rm_rf entry

publish :: IO ()
publish = shelly $ do
  -- 1. Capture current branch
  curBranch <- T.strip <$> run "git" ["rev-parse", "--abbrev-ref", "HEAD"]
  echo $ "Current branch: " <> curBranch

  -- 2. Rebuild
  echo "\n== Building site =="
  run_ "cabal" ["run", "site", "--", "build"]

  -- 3. Copy _site into a temp dir before any git operations
  echo "\n== Saving built site to temp directory =="
  withTmpDir $ \tmpDir -> do
    siteEntries <- ls "_site"
    forM_ siteEntries $ \entry -> cp_r entry tmpDir

    -- 4. Stash uncommitted changes
    echo "\n== Stashing uncommitted changes =="
    errExit False $ void $ run "git" ["stash", "--include-untracked", "--quiet"]

    let restore = do
          echo "\n== Restoring original branch =="
          errExit False $ void $ run "git" ["checkout", curBranch]
          errExit False $ void $ run "git" ["stash", "pop", "--quiet"]

    -- 5. Switch to gh-pages
    echo "\n== Switching to gh-pages =="
    catchany_sh (run_ "git" ["checkout", "gh-pages"]) $ \e -> do
      echo $ "  ✗ Failed to checkout gh-pages: " <> T.pack (show e)
      restore
      liftIO $ exitWith (ExitFailure 1)

    -- 6. Nuke everything except .git
    echo "\n== Replacing gh-pages content with built site =="
    cleanExceptGit "."

    -- 7. Copy from temp dir into repo root
    tmpEntries <- ls tmpDir
    forM_ tmpEntries $ \entry -> cp_r entry "."

    -- 8. Commit and push
    echo "\n== Committing and pushing =="
    run_ "git" ["add", "-A"]
    errExit False $ void $ run "git" ["commit", "-m", "publish: site rebuild"]
    run_ "git" ["push", "origin", "gh-pages"]

    -- 9. Switch back to original branch
    echo "\n== Switching back =="
    run_ "git" ["checkout", curBranch]

    -- 10. Restore stash
    errExit False $ void $ run "git" ["stash", "pop", "--quiet"]

  echo "\n✓ Published!"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["build"]   -> shelly $ run_ "cabal" ["run", "site", "--", "build"]
    ["watch"]   -> shelly $ run_ "cabal" ["run", "site", "--", "watch"]
    ["clean"]   -> shelly $ run_ "cabal" ["run", "site", "--", "clean"]
    ["publish"] -> publish
    _           -> usage >> exitWith (ExitFailure 64)