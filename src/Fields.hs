module Fields where

import Hakyll
import Network.URI.Encode

twitterShareIntentField :: String    -- ^ Destination key
                        -> String    -- ^ site root url
                        -> String    -- ^ twitter account name to mention as "via"
                        -> Context a -- ^ Resulting context
twitterShareIntentField key root name = functionField key $ \(path:_) i -> do
  metadata <- getMetadata $ itemIdentifier i
  let url = encode $ root ++ path
      query qs f k = case lookupString k metadata of
                            Nothing -> ""
                            Just s -> "&" ++ qs ++ "=" ++ encode (f s)
  return $ "https://twitter.com/intent/tweet?via=" ++ name
           ++ query "text" id "title"
           ++ query "hashtags" (filter (/=' ')) "tags"
           ++ "&original_referer=" ++ url
           ++ "&url=" ++ url
