{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>), arr)

import Hakyll

staticPageCompiler = readPageCompiler >>> 
                     addDefaultFields >>>
                     arr applySelf

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

    match "templates/*" $ compile templateCompiler

    match (list ["index.html"]) $ do
        route   $ setExtension "html"
        compile $ staticPageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
