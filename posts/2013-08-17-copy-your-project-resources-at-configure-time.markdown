---
title: Copy Your Project Resources at Configure Time
description: A useful script I've whipped up to solve resources headache.
tags: fp, haskell
---



This is mainly a note to myself, but I think it's content it's worthwhile to
deserve a blog entry, and might save some headache to some other Haskeller as well.

## The problem
While rewriting my freelance project [Expiweb](http://www.expiweb.it)
in Haskell, I stumbled upon the following problem: my newly-created project 
uses `persistent`, which relies on TH. In one of my modules I have a snippet
like this:

``` haskell
share [mkPersist sqlSettings, mkMigrate "migrateAuth"]
  $(persistFileWith lowerCaseSettings (getSchemaFor "auth"))
```

Where `getSchema` is defined as such:

``` haskell
getSchemaFor :: String -> FilePath
getSchemaFor appName =
  let p = unsafePerformIO getDataDir
    in p ++ "/resources/db/" ++ appName ++ "/schema.txt"
```

This boils down to have the generated code to search, at *compile time*, for
a file called `schema.txt` inside a `resources` directory. The accurate reader
may notice I'm using `getDataDir`, to leverage the full power of Cabal. The
problem with this code is that we have a mismatch between the nature of our
code:

* The TH snippet generates and check code at *compile time*
* `getDataDir` is meant to be used at *run time*

The proof is that the aforementioned function is generated inside a file
called `Paths_yourprojectname.hs`, where `yourprojectname` is the name of
your Cabal project. So, in my case, Cabal creates, *at compile time, but during
the build step*, a module called `Paths_expiweb.hs`. So far so good, but now
if we try to compile our project, we end up with an error message like this:

``` haskell
src/Expiweb/Auth/Types.hs:27:5:
    Exception when trying to run compile-time code:
      /Users/adinapoli/programming/haskell/expiweb.hs/.hsenv
      /cabal/share/expiweb-0.0.0.1/resources/db/auth/schema.txt:
    openFile: does not exist (No su ch file or directory)
      Code: persistFileWith lowerCaseSettings (getSchemaFor "auth")
    In the second argument of `share', namely
      `$(persistFileWith lowerCaseSettings (getSchemaFor "auth"))'
    In the expression:
      share
        [mkPersist sqlSettings, mkMigrate "migrateAuth"]
        ($(persistFileWith lowerCaseSettings (getSchemaFor "auth")))
Failed to install expiweb-0.0.0.1
```

This, unsurprisingly, means that we need our `schema.txt` to be there at compile
time.

## A first attempt: Using data-files and/or extra-source-files
After googling a bit, the first thing I've tried to do was to use `data-files`
to instruct Cabal about the fact I have files I want to be copied over when
I install the package. The problem is that `data-files` works at *run time*!
In other terms, our package will still be broken. This way is a dead end.

## A second attempt: Using Paths_expiweb inside Setup.hs
The second idea I had was to exploit the generated module `Paths_expiweb` 
inside a `Setup.hs` file, writing a `pre-build` hook to copy all the files
I need using the aforementioned `getDataDir` function.
The code might look like this:

``` haskell
#!/usr/bin/env runhaskell
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Package
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Verbosity
import Distribution.Simple.Utils

import Paths_expiweb

main :: IO ()
main = defaultMainWithHooks myHooks
  where myHooks = simpleUserHooks { preBuild = copyResources }

copyResources :: Args 
              -> BuildFlags
              -> IO HookedBuildInfo
copyResources args flags = do
  installDir <- getDataDir
  installDirectoryContents verbose "resources" (installDir ++ "/resources")
  return emptyHookedBuildInfo
```

This code seems to work, until you try to install the project inside a prestine
environment (e.g. your production server): the problem is a classical *circular
dependency* one: `Paths_expiweb` is created at *build time*, but we need to 
use it in our `Setup.hs` *before* it was even created (the proof is that, well,
we need to import it, and `Setup.hs` needs to typecheck and compile before we
can do anything)! So what?

## My final solution
The final solution is something similar, which I created shamelessly copying
a script I've found in the [Haskell Cafè](http://comments.gmane.org/gmane.comp.lang.haskell.cafe/106000).
**Disclaimer: This will work only if you are using hsenv, but it's simple to
adapt to other scenarios**:

``` haskell
#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Package
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Verbosity
import Distribution.Simple.Utils
import Distribution.PackageDescription.Parse (readPackageDescription)
import Data.Version (showVersion)

import System.Environment (lookupEnv)

-- | Expiweb package version, obtained parsing the cabal manifest file.
pkgVersion' :: IO String
pkgVersion' = do
  fmap (showVersion . pkgVersion . package . packageDescription)
    $ readPackageDescription silent "expiweb.cabal"

-------------------------------------------------------------------------------
-- | My version of getDataDir, to use at configure time.
getDataDir' :: IO FilePath
getDataDir' = do
    hsenvPath <- lookupEnv "HSENV"
    case hsenvPath of
      Nothing -> error "You need hsenv installed to install expiweb."
      (Just hPath) -> do
          version <- pkgVersion'
          return $ hPath ++ "/.hsenv/cabal/share/expiweb-" ++ version

main :: IO ()
main = defaultMainWithHooks expiwebHooks
  where expiwebHooks = simpleUserHooks { preBuild = copyExpiwebResources }

copyExpiwebResources :: Args 
                     -> BuildFlags
                     -> IO HookedBuildInfo
copyExpiwebResources args flags = do
  installDir <- getDataDir'
  installDirectoryContents verbose "resources" (installDir ++ "/resources")
  return emptyHookedBuildInfo
```

The trick is simple: We know that, under `hsenv`, `cabal` will install package
resources under `/cabal/share/packagename-packageversion`, so all we need is:

* Get the absolute path of our `hsenv` environment, looking up its variable
  in the environment

* Get the current package version, to allow the script to work across different
  versions. This is achieved using the `pkgVersion'` function, which parses the
  project manifest.

That's all. Further food of thoughts:

* Make the function work with everything, not just `hsenv`
* Remove the hardcoded name of the package, making it fully generic
* Make the function to work against hsenv "named environments" (e.g. `.hsenv_myenv`)

## References
This is a collection of useful posts which helped me during my woes:

* http://neilmitchell.blogspot.it/2008/02/adding-data-files-using-cabal.html
* http://comments.gmane.org/gmane.comp.lang.haskell.cafe/106000
