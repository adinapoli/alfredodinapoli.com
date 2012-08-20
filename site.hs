{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>), arr)
import Data.Monoid (mempty)

import Hakyll

staticPageCompiler :: Compiler Resource (Page String)
staticPageCompiler = readPageCompiler >>>
                     addDefaultFields >>>
                     arr applySelf

rootRoute :: Routes
rootRoute = gsubRoute "content/" (const "")

cvRoute :: Routes
cvRoute = gsubRoute "cv/" (const "") `composeRoutes` setExtension "html"

renderTagList' :: Compiler (Tags String) String
renderTagList' = renderTagList tagIdentifier

tagIdentifier :: String -> Identifier (Page String)
tagIdentifier = fromCapture "tags/*"

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA posts
        >>> pageListCompiler recentFirst "templates/postitem.html"
        >>> arr (copyBodyToField "posts" . fromBody)
        >>> arr (setField "title" ("Posts tagged " ++ tag))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

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

  -- Render each and every post
  match "posts/*" $ do
        route $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

   -- Post list
  match "posts.html" $ route idRoute
  create "posts.html" $ constA mempty
      >>> arr (setField "title" "Posts")
      >>> setFieldPageList recentFirst
              "templates/postitem.html" "posts" "posts/*"
      >>> applyTemplateCompiler "templates/posts.html"
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

  -- Tags
  create "tags" $
      requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

  -- Render RSS feed
  match "rss.xml" $ route idRoute
  create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration

  -- Add a tag list compiler for every tag
  match "tags/*" $ route $ setExtension ".html"
  metaCompile $ require_ "tags"
      >>> arr tagsMap
      >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

  match "templates/*" $ compile templateCompiler

  match "cv/*" $ do
    route cvRoute
    compile $ pageCompiler
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

  match "content/*" $ do
    route rootRoute
    compile $ staticPageCompiler
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Alfredo Di Napoli's Tech Blog"
    , feedDescription = "Personal blog of Alfredo Di Napoli"
    , feedAuthorName  = "Alfredo Di Napoli"
    , feedAuthorEmail = "alfredo.dinapoli@gmail.com"
    , feedRoot        = "http://www.alfredodinapoli.com"
    }
