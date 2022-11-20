{-# LANGUAGE DeriveGeneric #-}

module Types (
    Actors (..),
    Programs (..),
    Record (..),
    Records (..)
) where

import GHC.Generics

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

import Data.Aeson

data Actors = Actors {
    fullname_ :: String,
    gender_ :: String,
    birthday_ :: String,
    fk_program :: Int
} deriving (Show)

data Programs = Programs {
    id_ :: Int,
    program_ :: String,
    genre_ ::  String,
    status_ ::  String,
    rating_ :: Maybe Int
} deriving (Show)

data Record = Record {
    fullname :: String,
    gender :: String,
    birthday :: String,
    program :: String,
    genre ::  String,
    status ::  String,
    rating :: Maybe Int
} deriving (Show, Generic)

data Records = Records {
    records :: [Record]
} deriving (Show, Generic)

{-- Making above datatype instances of FromRow and ToRow type classes --}

instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Programs where
    fromRow = Programs <$> field <*> field <*> field <*> field <*> field

instance ToRow Programs where
    toRow (Programs i program genr stat rate)
        = toRow (i, program, genr, stat, rate)

instance FromRow Actors where
    fromRow = Actors <$> field <*> field <*> field <*> field

instance ToRow Actors where
    toRow (Actors f g b fk_p)
        = toRow (f, g, b, fk_p)

{-- Making above datatype instances of FromJSON type class --}

renameFields :: String -> String
renameFields "program" = "name"
renameFields "genre" = "genres" 
renameFields "fullname" = "name"
renameFields other = other

customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = renameFields
}

instance FromJSON Record where
    parseJSON = genericParseJSON customOptions

instance FromJSON Records
