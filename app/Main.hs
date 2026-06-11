{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.List (intersperse)
import System.Directory (listDirectory, removeFile)
import System.FilePath ((</>), takeExtension)

import Hakyll
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html (Html, (!))

removeOAndHiFiles :: Rules ()
removeOAndHiFiles = preprocess $ do
  contents <- listDirectory "posts"
  forM_ contents $ \f ->
    case takeExtension f of
      ".o"  -> removeFile ("posts" </> f)
      ".hi" -> removeFile ("posts" </> f)
      _     -> return ()

-- Probably wrong.
staticPageCompiler :: Compiler (Item String)
staticPageCompiler = getResourceBody

rootRoute :: Routes
rootRoute = gsubRoute "content/" (const "")

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

  match "css/fonts/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler


  match "doc/*" $ do
    route   idRoute
    compile copyFileCompiler

  -- Static content pages
  -- 404: full-bleed (no .page wrapper)
  match ("content/404.html") $ do
    route rootRoute
    compile $ staticPageCompiler
        >>= loadAndApplyTemplate "templates/default.html"
              (katexCtx <> defaultContext)
        >>= relativizeUrls

  -- All other content/*.html pages (index, contacts, oss) use the page shell
  match ("content/*.html") $ do
    route rootRoute
    compile $ staticPageCompiler
        >>= loadAndApplyTemplate "templates/page.html" (katexCtx <> defaultContext)
        >>= loadAndApplyTemplate "templates/default.html"
              (katexCtx <> defaultContext)
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
          compile $ take 10 <$> (recentFirst =<< loadAllSnapshots pattern "content")
              >>= renderAtom (feedConfiguration title) feedCtx

  match ("posts/*.o" .||. "posts/*.hi") $ removeOAndHiFiles

  -- Render each and every post
  match ("posts/*.markdown" .||. "posts/*.lhs" .||. "drafts/*.markdown" .||. "drafts/*.lhs") $ do
    route $ setExtension ".html"
    compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= return . fmap demoteHeaders
          >>= loadAndApplyTemplate "templates/post.html" (katexCtx <> postCtx tags)
          >>= loadAndApplyTemplate "templates/default.html" (katexCtx <> defaultContext)
          >>= relativizeUrls

  -- Post list
  create ["posts.html"] $ do
      route idRoute
      compile $ do
          list <- postList tags "posts/*" recentFirst
          makeItem ""
              >>= loadAndApplyTemplate "templates/posts.html"
                (constField "title" "Posts" <>
                 constField "posts" list <>
                 constField "isposts" "true" <>
                 defaultContext)
              >>= loadAndApplyTemplate "templates/default.html"
                (constField "title" "Posts" <> defaultContext)
              >>= relativizeUrls

  create ["drafts.html"] $ do
      route idRoute
      compile $ do
          list <- postList tags "drafts/*" recentFirst
          makeItem ""
              >>= loadAndApplyTemplate "templates/posts.html"
                (constField "title" "Drafts" <>
                 constField "posts" list <>
                 defaultContext)
              >>= loadAndApplyTemplate "templates/default.html"
                (constField "title" "Drafts" <> defaultContext)
              >>= relativizeUrls

  -- Render RSS feed
  create ["rss.xml"] $ do
      route idRoute
      compile $ take 10 <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")
              >>= renderAtom (feedConfiguration "All posts") feedCtx

  match "templates/*" $ compile templateCompiler

-------------------------------------------------------------------------------
-- Ausiliary functions
postCtx :: Tags -> Context String
postCtx tags = mconcat [ modificationTimeField "mtime" "%U"
                       , dateField "date" "%Y-%m-%d"
                       , tagsFieldWith getPostTags renderTag mconcat "tags" tags
                       , defaultContext ]

-- Get the tags for the current post. Hakyll's Tags stores the global map;
-- the post's own tags come from its front matter under 'tags'.
getPostTags :: Identifier -> Compiler [String]
getPostTags ident = do
    metadata <- getMetadata ident
    return $ case lookupString "tags" metadata of
        Nothing  -> []
        Just ts  -> words (map (\c -> if c == ',' then ' ' else c) ts)

-- Render a single tag as a chip. Unknown tags get chip--default.
-- The new post taxonomy: haskell, types, ai, compilers, security.
renderTag :: String -> Maybe FilePath -> Maybe Html
renderTag _ Nothing  = Nothing
renderTag tag (Just url) = Just $
    H.a ! A.href (H.toValue url)
        ! A.class_ (H.toValue ("chip chip--" <> tagColor tag))
        $ H.toHtml (tag :: String)

-- Map a tag name to a chip color class. The post taxonomy predates the
-- chip system, so most existing tags fall back to default. New posts
-- should use the new taxonomy: haskell, types, ai, compilers, security.
tagColor :: String -> String
tagColor "haskell"   = "haskell"
tagColor "ghc"       = "haskell"
tagColor "snap"      = "haskell"
tagColor "aws"       = "haskell"
tagColor "zurihac"   = "haskell"
tagColor "types"     = "types"
tagColor "type-families" = "types"
tagColor "fp"        = "types"
tagColor "scala"     = "types"
tagColor "ocaml"     = "types"
tagColor "ai"        = "ai"
tagColor "compilers" = "compilers"
tagColor "zig"       = "compilers"
tagColor "llvm"      = "compilers"
tagColor "grin"      = "compilers"
tagColor "security"  = "security"
tagColor "rust"      = "compilers"
tagColor "vim"       = "default"
tagColor "self"      = "default"
tagColor "life"      = "default"
tagColor "programming" = "default"
tagColor "linux"     = "default"
tagColor "macosx"    = "default"
tagColor "bodhi"     = "default"
tagColor "devops"    = "default"
tagColor _           = "default"

-------------------------------------------------------------------------------
postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts       <- loadAll pattern >>= preprocess'
    applyTemplateList postItemTpl (postCtx tags) posts


-------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

-------------------------------------------------------------------------------
feedConfiguration :: String -> FeedConfiguration
feedConfiguration _ = FeedConfiguration
    { feedTitle       = "Alfredo Di Napoli's Tech Blog"
    , feedDescription = "Personal blog of Alfredo Di Napoli"
    , feedAuthorName  = "Alfredo Di Napoli"
    , feedAuthorEmail = "alfredo.dinapoli@gmail.com"
    , feedRoot        = "https://www.alfredodinapoli.com"
    }

katexCtx :: Context a
katexCtx = field "katex" $ \item -> do
    katex <- getMetadataField (itemIdentifier item) "katex"
    return $ case katex of
                    Just x | x == "true" || x == "on" ->
                        "<link rel=\"stylesheet\" href=\"/css/katex.min.css\">\n\
                        \<script type=\"text/javascript\" src=\"/js/katex.min.js\"></script>\n\
                        \<script src=\"/js/auto-render.min.js\"></script>\n\
                        \<script src=\"/js/runkatex.js\"></script>"
                    _ -> mempty
