{-# LANGUAGE DeriveGeneric #-}

module Types (
    Entry (..),
    Program (..),
    Record (..),
    Records (..)
) where

import GHC.Generics

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

import Data.Aeson

data Entry = Entry {
    rating_ :: Float,
    summary_ :: String,
    runtime_ :: Int,
    premiered_ :: String,
    ended_ :: String,
    language_ :: String,
    fk_program :: Int
} deriving (Show)

data Program = Program {
    id_ :: Int,
    program_ :: String,
    genres_ ::  String,
    status_ :: String
} deriving (Show)

data Record = Record {
    rating :: Float,
    summary :: String,
    runtime :: Int,
    premiered :: String,
    ended :: String,
    language :: String,
    program :: String,
    genres ::  String,
    status :: String
} deriving (Show, Generic)

data Records = Records {
    records :: [Record]
} deriving (Show, Generic)

{-- Making above datatype instances of FromRow and ToRow type classes --}

instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Program where
    fromRow = Program <$> field <*> field <*> field <*> field

instance ToRow Program where
    toRow (Program i prog genr stat)
        = toRow (i, prog, genr, stat)

instance FromRow Entry where
    fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Entry where
    toRow (Entry ra s rt p e l fk_p)
        = toRow (ra, s, rt, p, e, l, fk_p)

{-- Making above datatype instances of FromJSON type class --}

renameFields :: String -> String
renameFields "program" = "name"
renameFields other = other

customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = renameFields
}

instance FromJSON Record where
    parseJSON = genericParseJSON customOptions

instance FromJSON Records
