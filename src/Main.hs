{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Fields

cfg :: Configuration
cfg = defaultConfiguration
        { deployCommand = "./publish.sh"
        }

static :: Rules ()
static = do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

pages :: Rules ()
pages = match (fromList ["about.md"]) $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
    >>= relativizeUrls

postTags :: Rules Tags
postTags = buildTags "posts/*.md" (fromCapture "tags/*.html")

postPages :: Tags -> Rules ()
postPages tags = match "posts/*.md" $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
    >>= relativizeUrls

tagPages :: Tags -> Rules ()
tagPages tags = tagsRules tags $ \tag ptrn -> do
  let title = "&#35;" ++ tag
  postList ptrn title "templates/tag.html"

index :: Rules ()
index = create ["index.html"] $
  postList "posts/*" "Blog" "templates/index.html"

templates :: Rules ()
templates = match "templates/*" $ compile templateBodyCompiler

postList :: Pattern -> String -> Identifier -> Rules ()
postList ptrn title tmpl = do
  route idRoute
  compile $ do
    posts <- recentFirst =<< loadAll ptrn
    let ctx = listField "posts" postCtx (return posts)
              <> constField "title" title
              <> defaultContext
    makeItem ""
      >>= loadAndApplyTemplate tmpl ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
          <> twitterShareIntentField "twitter" "https://www.poberezkin.com" "epoberezkin"
          <> tagField "talk" "talk"
          <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags
                       <> postCtx

feeds :: Rules ()
feeds = do
  feed renderAtom "feed.atom"
  feed renderRss "feed.rss"
  where
    getPosts = recentFirst =<< loadAllSnapshots "posts/*" "content"
    config = FeedConfiguration
              { feedAuthorEmail = "evgeny@poberezkin.com"
              , feedAuthorName = "Evgeny Poberezkin"
              , feedDescription = "Evgeny Poberezkin's blog"
              , feedRoot = "http://www.poberezkin.com"
              , feedTitle = "Evgeny Poberezkin"
              }
    ctx = bodyField "description"
          <> postCtx
    feed render name = create [name] $ do
      route idRoute
      compile $ do
        posts <- getPosts
        render config ctx posts

main :: IO ()
main = hakyllWith cfg $ do
  static
  pages
  tags <- postTags
  postPages tags
  tagPages tags
  feeds
  index
  templates
