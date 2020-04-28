import Hakyll

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
pages = match (fromList ["about.md", "contact.md"]) $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
    >>= relativizeUrls

postTags :: Rules Tags
postTags = buildTags "posts/*" (fromCapture "tags/*.html")

postPages :: Tags -> Rules ()
postPages tags = match "posts/*" $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
    >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
    >>= relativizeUrls

tagPages :: Tags -> Rules ()
tagPages tags = tagsRules tags $ \tag ptrn -> do
  let title = "&#35;" ++ tag ++ " posts"
  postList ptrn title "templates/tag.html"

archive :: Rules ()
archive = create ["archive.html"] $
  postList "posts/*" "Archives" "templates/archive.html"

postList :: Pattern -> String -> Identifier -> Rules ()
postList ptrn title tmpl = do
  route idRoute
  compile $ do
    posts <- recentFirst =<< loadAll ptrn
    let ctx = listField "posts" postCtx (return posts)
              `mappend` constField "title" title
              `mappend` defaultContext
    makeItem ""
      >>= loadAndApplyTemplate tmpl ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls

index :: Rules ()
index = match "index.html" $ do
  route idRoute
  compile $ do
    posts <- recentFirst =<< loadAll "posts/*"
    let indexCtx = listField "posts" postCtx (return posts)
                   `mappend` defaultContext
    getResourceBody
      >>= applyAsTemplate indexCtx
      >>= loadAndApplyTemplate "templates/default.html" indexCtx
      >>= relativizeUrls

templates :: Rules ()
templates = match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
          `mappend` defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags
                       `mappend` postCtx

main :: IO ()
main = hakyllWith cfg $ do
  static
  pages
  tags <- postTags
  postPages tags
  tagPages tags
  archive
  index
  templates
