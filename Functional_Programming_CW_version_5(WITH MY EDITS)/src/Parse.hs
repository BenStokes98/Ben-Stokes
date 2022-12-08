{-|
Module      : Parse
Copyright   : (c) Ben Stokes, 2022
                  Navid Sheikh, 2022
-}


{-# LANGUAGE DeriveGeneric #-}

module Parse (
    parseRecords,
) where

import Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

-- * Parsing JSON

-- | Converting JSON record to Haskell 'Record' type 
parseRecords :: L8.ByteString -> Either String [Record]
parseRecords j = eitherDecode j :: Either String [Record]