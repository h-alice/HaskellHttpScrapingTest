{-# LANGUAGE OverloadedStrings #-}

module HandSomeScraper (fetchURL) where


import Text.HandsomeSoup
import Text.XML.HXT.Core
import qualified Data.Text as T  -- For text manipulation
import qualified Data.Text.Lazy.Encoding as TLE
import Network.Wreq
import Control.Lens ( (^.) )
import qualified Data.Text.Lazy as TL

-- 
fetchURL :: String -> IO (Either String [T.Text])
fetchURL url = do
    -- Make a GET request.
    response <- get url

    -- Check for successful response.
    if response ^. responseStatus . statusCode /= 200
        then return $ Left $ "Failed to fetch the URL, status code: " ++ show (response ^. responseStatus)
        else do
            -- Parse HTML content using HandsomeSoup.
            let doc = parseHtml  $ TL.unpack $ TLE.decodeUtf8 (response ^. responseBody)

            -- Extract product titles.
            titles <- runX $ doc >>> css "div.caption" >>> css "h4" >>> css "a" >>> deep getText
            
            return $ Right $ map (T.strip . T.pack) titles