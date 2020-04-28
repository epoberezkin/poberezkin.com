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

postPages :: Rules ()
postPages = match "posts/*" $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/post.html" postCtx
    >>= loadAndApplyTemplate "templates/default.html" postCtx
    >>= relativizeUrls

archive :: Rules ()
archive = create ["archive.html"] $ do
  route idRoute
  compile $ do
    posts <- recentFirst =<< loadAll "posts/*"
    let archiveCtx = listField "posts" postCtx (return posts)
                     `mappend` constField "title" "Archives"
                     `mappend` defaultContext
    makeItem ""
      >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
      >>= loadAndApplyTemplate "templates/default.html" archiveCtx
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

main :: IO ()
main = hakyllWith cfg $ do
  static
  pages
  postPages
  archive
  index
  templates
