{-# LANGUAGE OverloadedStrings #-}

module XmlScraper (fetchURL) where

import qualified Data.Text as T  -- For text manipulation
  -- To handle text input/output
import Network.HTTP.Simple  -- For HTTP requests
import Text.HTML.DOM  -- To parse HTML documents
import Text.XML.Cursor  -- For navigating parsed HTML
-- 
fetchURL :: String -> IO [T.Text]
fetchURL url = do
    request <- parseRequest url
    response <- httpLBS request
    let doc = parseLBS $ getResponseBody response
        cursor = fromDocument doc in
        return $ cursor $// element "div" >=> attributeIs "class" "caption"
                                          >=> child
                                          >=> element "h4"
                                          >=> child
                                          >=> attributeIs "class" "title" &// content
