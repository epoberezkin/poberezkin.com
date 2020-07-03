module Fields where

import Hakyll
import Data.Char
import Data.List.Split
import Data.Maybe
import Network.URI.Encode

-- | Creates a function 'field' to use as href in twitter share intent link (without twitter JS).
-- Post $url$ should be passed as a parameter to this field.
twitterShareIntentField :: String    -- ^ Destination key
                        -> String    -- ^ site root url
                        -> String    -- ^ twitter account name to mention as "via"
                        -> Context a -- ^ Resulting context
twitterShareIntentField key root name = functionField key shareIntent
  where
    shareIntent :: [String] -> Item a -> Compiler String
    shareIntent [] _ = return ""
    shareIntent (path:_) i = do
      metadata <- getMetadata $ itemIdentifier i
      let url = encode $ root ++ path
          query qs f k = case lookupString k metadata of
                                Nothing -> ""
                                Just s -> "&" ++ qs ++ "=" ++ encode (f s)
      return $ "https://twitter.com/intent/tweet?via=" ++ name
              ++ query "text" id "title"
              ++ query "hashtags" (filter (not . isSpace)) "tags"
              ++ "&original_referer=" ++ url
              ++ "&url=" ++ url

-- | Creates a function 'field' to use as href in reddit share intent link (without any JS).
-- Post $url$ should be passed as a parameter to this field.
redditShareIntentField :: String    -- ^ Destination key
                       -> String    -- ^ site root url
                       -> Context a -- ^ Resulting context
redditShareIntentField key root = functionField key shareIntent
  where
    shareIntent :: [String] -> Item a -> Compiler String
    shareIntent [] _ = return ""
    shareIntent (path:_) i = do
      metadata <- getMetadata $ itemIdentifier i
      let url = encode $ root ++ path
          submit = "submit?url=" ++ url ++ "&title=" ++ fromJust (lookupString "title" metadata)
      return $ "https://www.reddit.com/" ++ fromMaybe submit (lookupString "reddit" metadata)

-- | Creates a 'field' that checks post tag presence to use with the $if()$ template macro.
tagField :: String    -- ^ Destination key
         -> String    -- ^ Post tag to filter
         -> Context a -- ^ Resulting context
tagField key tag = field key $ \i -> do
  tags <- getMetadataField (itemIdentifier i) "tags"
  let tagPresent = elem tag . splitOneOf ", " $ fromMaybe "" tags
  if tagPresent
    then return tag
    else noResult $ "Tag " ++ tag ++ "not present"
