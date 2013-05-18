module FeedItems (getUpdatedItems, saveUpdated, lastUpdated, getItemDateString) where

import Text.Feed.Import (parseFeedString)
import Text.Feed.Types (Item)
import Text.Feed.Query (feedItems, getItemDate)
import Data.Maybe (fromJust)
import System.Directory (doesFileExist)

parseItems :: String -> [Item]
parseItems feed = feedItems . fromJust . parseFeedString $ feed

updatedItems :: String -> [Item] -> [Item]
updatedItems time items = filter (\x -> isNew time x) items

isNew :: String -> Item -> Bool
isNew time item = time < (getItemDateString item)

getItemDateString :: Item -> String
getItemDateString item = fromJust $ getItemDate item

lastUpdated :: String -> IO (Maybe String)
lastUpdated file = do
  exist <- doesFileExist file
  case exist of
    True -> do
      time <- readFile file
      return $ Just $ time
    False -> return Nothing

saveUpdated :: String -> [Item] -> IO ()
saveUpdated file items = case latestTime items of
  Just time -> writeFile file time
  Nothing   -> return ()

latestTime :: [Item] -> Maybe String
latestTime []    = Nothing
latestTime items =  Just $ maximum $ map getItemDateString items

getUpdatedItems :: Maybe String -> String -> [Item]
getUpdatedItems Nothing     feed = updatedItems "0" $ parseItems feed
getUpdatedItems (Just time) feed = updatedItems time $ parseItems feed

