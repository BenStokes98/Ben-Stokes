{-|
Module      : Types
Copyright   : (c) Ben Stokes, 2022
                  Navid Sheikh, 2022
-}

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass, TypeFamilies #-}


module Types (
    Entry (..),
    Program (..),
    Summary (..),
    Record (..),
    ShowWithDate(..),
    RandomSummary (..),
    renameFields,
    customOptions
) where

-- | import the necessary libraries, data type and function 
import GHC.Generics
import Data.Aeson
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

-- * Data Type

-- | The Entry data type is used to create rows in the entries table.
-- Use case : Database.createRecord
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

-- | The Program data type is used to support the conversion for SQL operations.
--
-- The Program data type is used to create rows in the program table.
--
-- Use case : Database.queryProgramAllEntries,  Database.queryProgramSummary 
data Program = Program {
    id_ :: Maybe Int,
    program_ :: String
} deriving (Show)



-- | The Summary data type is used to support the conversion for SQL operations.
--
-- The Summary data type is used to create rows in the summary table.
--
-- Use case : Database.createSummary,  Database.query5RandomPrograms
data Summary = Summary {
    summary_ :: Maybe String,
    fk_program2 :: Maybe Int
} deriving (Show)

-- | The RandomSummary data type is used to support the random mini game and SQL operaions.
--
-- Use case : Database.queryProgramRandomSummary
data RandomSummary = RandomSummary {
    guess_summary :: Maybe String,
    guess_status :: Maybe String
} deriving (Show)

-- | The ShowWithDate data type is used to support SQL operaions that includes dates.
--
-- Use case : Database.queryAllProgramEnded, Database.queryRunningProgram, Database.queryProgramPremieredDate
data ShowWithDate = ShowWithDate {
    show_id :: Maybe Int,
    show_title :: String,
    show_date :: Maybe String
} deriving (Show)

-- | Used to convert raw json data into readable data type in Haskell
--
--  Use Case: Database.saveRecordsEntries, Database.saveRecordsSummary

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


-- * Constructors
instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Program where
    fromRow = Program <$> field <*> field 

instance FromRow ShowWithDate where
    fromRow = ShowWithDate <$> field <*> field <*> field 

instance FromRow Summary where
    fromRow = Summary <$> field <*> field

instance FromRow RandomSummary where
    fromRow = RandomSummary <$> field <*> field

instance FromRow Entry where
    fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Entry where
    toRow (Entry w rt p e l si st fk_p1)
        = toRow (w, rt, p, e, l, si, st, fk_p1)

instance ToRow Summary where
    toRow (Summary s fk_p2)
        = toRow (s, fk_p2)

instance ToRow RandomSummary where
    toRow (RandomSummary r_s r_st)
        = toRow (r_s, r_st)

instance ToRow Program where
    toRow (Program i prog)
        = toRow (i, prog)

instance ToRow ShowWithDate where
    toRow (ShowWithDate s_i s_t s_d)
        = toRow (s_i, s_t, s_d)

-- * JSON Settings 

-- | Rename certiain fields to imporove readibility and resolve naming ambiguity
renameFields :: String -> String
renameFields "program" = "name"
renameFields "site" = "officialSite"
renameFields other = other

-- | Setting default options for 'FromJSON' 
--
-- omitNothingFields = True - Null values are now considered.
customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = renameFields,
    omitNothingFields = True
}

instance FromJSON Record where
    parseJSON = genericParseJSON customOptions