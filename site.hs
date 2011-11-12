{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>), arr, (>>^))

import Hakyll
import Hakyll.Core.Writable.CopyFile (CopyFile)

staticPageCompiler = readPageCompiler >>> 
                     addDefaultFields >>>
                     arr applySelf
                     
                     
copyContentCompiler :: Compiler Resource CopyFile
copyContentCompiler = getIdentifier >>^ CopyFile . (\x -> "")

main :: IO ()
main = hakyll $ do
  
  match "content/*" $ do
    route idRoute
    compile copyContentCompiler
  
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

  match (list ["index.html", "portfolio.html", "whereiam.html", "hireme.html"]) $ do
    route   $ setExtension "html"
    compile $ staticPageCompiler
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler
