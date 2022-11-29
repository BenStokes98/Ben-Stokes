{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass, TypeFamilies #-}


module Types (
    Entry (..),
    Program (..),
    Summary (..),
    Record (..)
) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

data Entry = Entry {
    weight_ :: Maybe Int,
    runtime_ :: Maybe Int,
    premiered_ :: Maybe String,
    ended_ :: Maybe String,
    language_ :: Maybe String,
    site_ :: Maybe String,
    status_ :: Maybe String,
    fk_program1 :: Maybe Int
} deriving (Show)



data Program = Program {
    id_ :: Maybe Int,
    program_ :: String
} deriving (Show)

data Summary = Summary {
    summary_ :: Maybe String,
    fk_program2 :: Maybe Int
} deriving (Show)

data Record = Record {
    weight :: Maybe Int,
    runtime :: Maybe Int,
    premiered :: Maybe String,
    ended :: Maybe String,
    language :: Maybe String,
    site :: Maybe String,
    status :: Maybe String,
    program :: String,
    summary :: Maybe String
} deriving (Show, Generic)


--data Summary = Summary {summary :: String}

{-- Making above datatype instances of FromRow and ToRow type classes --}

instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Program where
    fromRow = Program <$> field <*> field 
    --fromRow = Program <$> field

instance ToRow Program where
    toRow (Program i prog)
        = toRow (i, prog)

instance FromRow Summary where
    fromRow = Summary <$> field <*> field

instance FromRow Entry where
    fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Entry where
    toRow (Entry w rt p e l si st fk_p1)
        = toRow (w, rt, p, e, l, si, st, fk_p1)

instance ToRow Summary where
    toRow (Summary s fk_p2)
        = toRow (s, fk_p2)

-- instance ToRow Summary where
--     toRow (Summary s fk_p)
--         = toRow (s, fk_p)

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
