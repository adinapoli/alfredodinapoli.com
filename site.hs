{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Data.Monoid (mappend, mconcat)

import Hakyll

-- Probably wrong.
staticPageCompiler :: Compiler (Item String)
staticPageCompiler = getResourceBody

rootRoute :: Routes
rootRoute = gsubRoute "content/" (const "")

cvRoute :: Routes
cvRoute = gsubRoute "cv/" (const "") `composeRoutes` setExtension "html"


main :: IO ()
main = hakyll $ do

  match "img/*/*" $ do
    route   idRoute
    compile copyFileCompiler

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

  -- Static files
  match "content/*" $ do
    route rootRoute
    compile $ do
        staticPageCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "cv_eu/*" $ do
    route rootRoute
    compile $ staticPageCompiler
      >>= loadAndApplyTemplate "templates/cv_eu.html" defaultContext
      >>= relativizeUrls

  -- Build tags
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged " ++ tag

      -- Copied from posts, need to refactor
      route idRoute
      compile $ do
          list <- postList tags pattern recentFirst
          makeItem ""
              >>= loadAndApplyTemplate "templates/posts.html"
                      (constField "title" title `mappend`
                          constField "posts" list `mappend`
                          defaultContext)
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls

      -- Create RSS feed as well
      version "rss" $ do
          route   $ setExtension "xml"
          compile $ loadAllSnapshots pattern "content"
              >>= return . take 10 . recentFirst
              >>= renderAtom (feedConfiguration title) feedCtx

  -- Render each and every post
  match "posts/*" $ do
    route $ setExtension ".html"
    compile $ do
        pandocCompiler
          >>= saveSnapshot "content"
          >>= return . fmap demoteHeaders
          >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
          >>= loadAndApplyTemplate "templates/default_nosidebar.html" defaultContext
          >>= relativizeUrls

  -- Post list
  create ["posts.html"] $ do
      route idRoute
      compile $ do
          list <- postList tags "posts/*" recentFirst
          makeItem ""
              >>= loadAndApplyTemplate "templates/posts.html"
                (constField "title" "Posts" `mappend`
                 constField "posts" list `mappend`
                 defaultContext)
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls


  -- Render RSS feed
  create ["rss.xml"] $ do
      route idRoute
      compile $ do
          loadAllSnapshots "posts/*" "content"
              >>= return . take 10 . recentFirst
              >>= renderAtom (feedConfiguration "All posts") feedCtx

  match "templates/*" $ compile templateCompiler

  match "cv/*" $ do
    route cvRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls


-------------------------------------------------------------------------------
-- Ausiliary functions
postCtx :: Tags -> Context String
postCtx tags = mconcat [ modificationTimeField "mtime" "%U"
                       , dateField "date" "%B %e  %Y"
                       , tagsField "tags" tags
                       , defaultContext ]

-------------------------------------------------------------------------------
postList :: Tags -> Pattern -> ([Item String] -> [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts       <- preprocess' <$> loadAll pattern
    applyTemplateList postItemTpl (postCtx tags) posts

-------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

-------------------------------------------------------------------------------
feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "Alfredo Di Napoli's Tech Blog"
    , feedDescription = "Personal blog of Alfredo Di Napoli"
    , feedAuthorName  = "Alfredo Di Napoli"
    , feedAuthorEmail = "alfredo.dinapoli@gmail.com"
    , feedRoot        = "http://www.alfredodinapoli.com"
    }
