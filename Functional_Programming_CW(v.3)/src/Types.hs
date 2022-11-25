{-# LANGUAGE DeriveGeneric   #-}



module Types (
    Entry (..),
    Program (..),
    Record (..),
    Records (..)
) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow


data Entry = Entry {
    weight_ :: Maybe Int,
    summary_ :: Maybe String,
    runtime_ :: Maybe Int,
    premiered_ :: Maybe String,
    ended_ :: Maybe String,
    language_ :: Maybe String,
    fk_program :: Maybe Int
} deriving (Show)

data Program = Program {
    id_ :: Maybe Int,
    program_ :: Maybe String,
    site_ :: Maybe String,
    status_ :: Maybe String
} deriving (Show)

data Record = Record {
    weight :: Maybe Int,
    summary :: Maybe String,
    runtime :: Maybe Int,
    premiered :: Maybe String,
    ended :: Maybe String,
    language :: Maybe String,
    program :: Maybe String,
    site :: Maybe String,
    status :: Maybe String
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
    toRow (Program i prog site stat)
        = toRow (i, prog, site, stat)

instance FromRow Entry where
    fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Entry where
    toRow (Entry w s rt p e l fk_p)
        = toRow (w, s, rt, p, e, l, fk_p)

{-- Making above datatype instances of FromJSON type class --}

renameFields :: String -> String
renameFields "program" = "name"
renameFields "site" = "officialSite"
renameFields other = other

customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = renameFields,
    omitNothingFields = True
}

instance FromJSON Record where
    parseJSON = genericParseJSON customOptions

--instance FromJSON [Record]

-- instance FromJSON a => FromJSON (Maybe a) where
--     FromJSON null = Nothing 
--     FromJSON Maybe a = a

--     idea: try first to parse as Null, if successful return Nothing
--         otherwise return Just (parse as a)
