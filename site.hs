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
pages = match (fromList ["about.md"]) $ do
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
  let title = "&#35;" ++ tag
  postList ptrn title "templates/tag.html"

blog :: Rules ()
blog = create ["blog.html"] $
  postList "posts/*" "Blog" "templates/blog.html"

index :: Rules ()
index = create ["index.html"] $
  postList "posts/*" "Welcome" "templates/index.html"

templates :: Rules ()
templates = match "templates/*" $ compile templateBodyCompiler

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
  blog
  index
  templates
