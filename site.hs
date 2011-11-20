{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>), arr, (>>^))

import Hakyll
import Hakyll.Core.Routes (gsubRoute)

staticPageCompiler = readPageCompiler >>> 
                     addDefaultFields >>>
                     arr applySelf

rootRoute = gsubRoute "content/" (const "")

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

  match "content/*" $ do
    route   $ rootRoute
    compile $ staticPageCompiler
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler
