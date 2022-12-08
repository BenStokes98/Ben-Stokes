{-|
Module      : Fetch
Copyright   : (c) Ben Stokes, 2022
                  Navid Sheikh, 2022
-}

module Fetch (
    -- * Fetching data
    --
    download
) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

type URL = String

-- | Request to URL and convert the response to String
download :: URL -> IO L8.ByteString
download url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response
