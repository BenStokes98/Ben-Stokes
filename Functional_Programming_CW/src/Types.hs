{-# LANGUAGE DeriveGeneric #-}

module Types (
    Entry (..),
    Country (..),
    Record (..),
    Records (..)
) where

import GHC.Generics

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

import Data.Aeson

data Entry = Entry {
    date_ :: String,
    day_ :: String,
    month_ :: String,
    year_ :: String,
    cases_ :: Int,
    deaths_ :: Int,
    fk_country :: Int
} deriving (Show)

data Country = Country {
    id_ :: Int,
    country_ :: String,
    continent_ ::  String,
    population_ :: Maybe Int
} deriving (Show)

data Record = Record {
    date :: String,
    day :: String,
    month :: String,
    year :: String,
    cases :: Int,
    deaths :: Int,
    country :: String,
    continent ::  String,
    population :: Maybe Int
} deriving (Show, Generic)

data Records = Records {
    records :: [Record]
} deriving (Show, Generic)

{-- Making above datatype instances of FromRow and ToRow type classes --}

instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Country where
    fromRow = Country <$> field <*> field <*> field <*> field

instance ToRow Country where
    toRow (Country i coun cont pop)
        = toRow (i, coun, cont, pop)

instance FromRow Entry where
    fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Entry where
    toRow (Entry dt dy m y c d fk_c)
        = toRow (dt, dy, m, y, c, d, fk_c)

{-- Making above datatype instances of FromJSON type class --}

renameFields :: String -> String
renameFields "date" = "dateRep"
renameFields "continent" = "continentExp" 
renameFields "country" = "countriesAndTerritories" 
renameFields "population" = "popData2019"
renameFields other = other

customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = renameFields
}

instance FromJSON Record where
    parseJSON = genericParseJSON customOptions

instance FromJSON Records
