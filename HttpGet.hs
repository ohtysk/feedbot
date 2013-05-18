module HttpGet (httpGet) where

import Network.HTTP (getHeaders, simpleHTTP, getRequest, getResponseCode, getResponseBody)
import Data.Either (rights)

httpGet :: String -> IO ((Int, Int, Int), [String], String)
httpGet url = do
  res <- simpleHTTP $ getRequest url
  code <- getResponseCode res
  let headers = getHeaders . head . rights $ [res]
  body <- getResponseBody res
  return (code, map show headers, body)
