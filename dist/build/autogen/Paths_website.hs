module Paths_website (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/adinapoli/programming/haskell/alfredodinapoli.com/.hsenv/cabal/bin"
libdir     = "/Users/adinapoli/programming/haskell/alfredodinapoli.com/.hsenv/cabal/lib/website-1.1.0.0/ghc-7.6.3"
datadir    = "/Users/adinapoli/programming/haskell/alfredodinapoli.com/.hsenv/cabal/share/website-1.1.0.0"
libexecdir = "/Users/adinapoli/programming/haskell/alfredodinapoli.com/.hsenv/cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "website_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "website_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "website_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "website_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
