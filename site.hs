{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>), arr, (>>^))

import Hakyll
import Hakyll.Core.Routes (gsubRoute)

staticPageCompiler = readPageCompiler >>>
                     addDefaultFields >>>
                     arr applySelf

rootRoute = gsubRoute "content/" (const "")
cvRoute = gsubRoute "cv/" (const "") `composeRoutes` setExtension "html"

main :: IO ()
main = hakyll $ do

  match "img/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "js/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "doc/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateCompiler

  match "cv/*" $ do
    route $ cvRoute
    compile $ pageCompiler
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

  match "content/*" $ do
    route   $ rootRoute
    compile $ staticPageCompiler
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler
