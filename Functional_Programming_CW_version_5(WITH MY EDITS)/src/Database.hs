{-|
Module      : Database
Copyright   : (c) Ben Stokes, 2022
                  Navid Sheikh, 2022
-}

{-# LANGUAGE OverloadedStrings #-}
-- or, on GHCI:
-- > :set -XOverloadedStrings
module Database (
    -- * Database Structure Setup
    --
    -- | Used to set up the entries, summary
    -- and program tables in SQLite.
    initialiseDB,
    -- * Database Insert 
    --
    -- | For insert of records into the newly created entries, summary
    -- and program tables. 
    getOrCreateProgram,
    createRecord,
    saveRecordsEntries,
    saveRecordsSummary,
    createSummary,
    -- * Error handling  
    --
    -- | Allows Maybe Int to be parsed and then can check to see if the input
    -- is of type Int. If not, an error will be thrown.
    maybeRead,
    -- * Random Int Generator 
    --
    -- | Used during the 'queryProgramRandomSummary' function to get the random value.
    getRandomInt,
    countRecords,
    countRecordsInt,
    -- * Query Programs 
    --
    -- | Functions created to query the database based on the users desired functionality.
    queryProgramAllEntries,
    queryProgramSummary,
    queryAllProgramEnded,
    queryProgramPremieredDate,
    query5RandomPrograms,
    queryProgramRandomSummary,
    countAllRunningProgram,
    queryRunningProgram
) where

import Types
import Database.SQLite.Simple
import System.Random
import Data.Maybe (listToMaybe)


-- | Method to create database shows.db using Database.SQLite.Simple.
initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "shows.db"
        execute_ conn "CREATE TABLE IF NOT EXISTS program (\
        \id INTEGER PRIMARY KEY AUTOINCREMENT,\
        \program VARCHAR(200) NOT NULL\
        \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS entries (\
        \weight INT,\
        \runtime INT,\
        \premiered DATE, \
        \ended DATE DEFAULT NULL,\
        \language INT DEFAULT NULL, \
        \site VARCHAR(300),\
        \status VARCHAR(80) DEFAULT NULL,\
        \fk_program INTEGER\
        \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS summary (\
        \summary VARCHAR(2000) NOT NULL,\
        \fk_program INTEGER\
        \)"
        return conn


-- | Insert records into program table. 
--
-- Able to prevent dulpication of records in the table.
getOrCreateProgram :: Connection -> String -> IO Program
getOrCreateProgram conn prog = do
    results <- queryNamed conn "SELECT * FROM program WHERE program=:program" [":program" := prog]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO program (program) VALUES (?)" [prog :: String]
        getOrCreateProgram conn prog


-- | Creates a record with type 'Entry' from the 'Record' values and inserts into the entries table.
createRecord :: Connection -> Record -> IO ()
createRecord conn record = do
    c <- getOrCreateProgram conn (program record)
    let entry = Entry {
        weight_ = weight record :: Maybe Int,
        runtime_ = runtime record :: Maybe Int,
        premiered_ = premiered record :: Maybe String,
        ended_ = ended record :: Maybe String,
        language_ = language record :: Maybe String,
        site_ = site record :: Maybe String,
        status_ = status record :: Maybe String,
        fk_program1 = id_ c :: Maybe Int
    }
    execute conn "INSERT INTO entries VALUES (?,?,?,?,?,?,?,?)" entry

-- | Creates a record with type 'Summary' from the 'Record' values and inserts into the entries table.
createSummary :: Connection -> Record -> IO ()
createSummary conn record  = do
    c <- getOrCreateProgram conn (program record)
    let insertsummary = Summary {
        summary_ = summary record :: Maybe String,
        fk_program2 = id_ c :: Maybe Int
    }
    execute conn "INSERT INTO summary VALUES (?,?)" insertsummary


-- | Converts a list of integers into a single (non-list) integer.
listToInt :: [Int] -> Int
listToInt [] = 0
listToInt (xs) = head(xs)



-- | Error handling for Integers  
--
-- Uses Import Data.Maybe (listToMaybe) for preset of 'listToMaybe' method.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads

-- | Invoke function to create entries records.
--
-- Calls upon the 'createRecord' funtion to complete the process.
saveRecordsEntries :: Connection -> [Record] -> IO ()
saveRecordsEntries conn = mapM_ (createRecord conn)

-- | Invoke function to create summary records.
--
-- Calls upon the 'createSummary' funtion to complete the process.
saveRecordsSummary :: Connection -> [Record] -> IO ()
saveRecordsSummary conn = mapM_ (createSummary conn)

-- | Display certain number of shows based on user input.
--
-- Includes error handeling which prevents users from entering non interger records, by using the 'maybeRead' method.
--
-- Output is of type 'Program' list.
queryProgramAllEntries :: Connection -> IO [Program]
queryProgramAllEntries conn = do
    putStrLn "-----------------------------------------"
    putStr "Enter how many shows you want to display  > "
    numberOfPrograms <- fmap maybeRead getLine :: IO (Maybe Int)
    maybe (putStrLn " \n\
                    \!!ERROR!!  Oops, you didn't enter a Integer! Please try again.  !!ERROR!! \n\ 
                    \")
          (putStrLn . ("Number of shows returned: " ++) . show)
          numberOfPrograms
    let sql = "SELECT  * FROM program  LIMIT ? "
    query conn sql [numberOfPrograms]

-- | Display the 4 random options + the solution record  to the game 
query5RandomPrograms :: Connection -> Int -> IO [Program]
query5RandomPrograms conn n = do
    putStrLn "-----------------------------------------"
    putStrLn "Pick amongst these programs : "
    let sql = "SELECT  * FROM program WHERE program.id = ? UNION SELECT * FROM (SELECT * FROM program ORDER BY RANDOM() LIMIT 4) ORDER BY id"
    query conn sql [n]

-- | Display the random summary and its status.
--
-- Output is a list of 'RandomSummary' records. 
queryProgramRandomSummary :: Connection -> Int -> IO [RandomSummary]
queryProgramRandomSummary conn randomID = do
    putStrLn "-----------------------------------------"
    putStrLn "Guess ID of this movie's summary : "
    let sql = "SELECT DISTINCT REPLACE(REPLACE(REPLACE(REPLACE(summary, '<p>', ''),'</p>', ''),'<b>', '') ,'</b>', '') , entries.status FROM summary, entries where summary.fk_program = ? AND entries.fk_program = ?"
    query conn sql [randomID, randomID]

-- | Show the summary of a particular show.
--
-- Includes error handeling which prevents users from entering non interger records, by using the 'maybeRead' method.
--
-- Output is of type 'Summary' list, to display only the show summary and record ID.
queryProgramSummary :: Connection -> IO [Summary]
queryProgramSummary  conn = do
    putStrLn "-----------------------------------------"
    putStr "Enter a program ID to display the summary > "
    id_show <- fmap maybeRead getLine :: IO (Maybe Int)
    maybe (putStrLn " \n\
                    \!!ERROR!!  Oops, you didn't enter a Integer! Please try again.  !!ERROR!! \n\ 
                    \")
          (putStrLn . ("Getting summary for program ID: " ++) . show)
          id_show
    let sql = "SELECT DISTINCT REPLACE(REPLACE(REPLACE(REPLACE(summary, '<p>', ''),'</p>', ''),'<b>', '') ,'</b>', ''), fk_program FROM summary where summary.fk_program = ?"
    query conn sql [id_show]

-- | Show all shows that have a status of Ended.
--
-- Displays output of type 'ShowWithDate'.
queryAllProgramEnded :: Connection -> IO [ShowWithDate]
queryAllProgramEnded conn = do
    let sql  = "SELECT program.id, program, ended FROM program, entries WHERE entries.fk_program == program.id  AND entries.status='Ended' "
    query_ conn sql 

-- | Show all the programs premiered before user defined year.
--
-- Displays output of type 'ShowWithDate'.
queryProgramPremieredDate ::  Connection -> IO[ShowWithDate]
queryProgramPremieredDate conn = do
    putStrLn "-----------------------------------------"
    putStr "Filter movie by year - Enter premier year  > "
    premieredYear <- getLine
    let sql  = "SELECT DISTINCT program.id, program, premiered FROM entries, program WHERE entries.fk_program == program.id AND STRFTIME('%Y', entries.premiered) < ? ORDER BY entries.premiered ASC "
    query conn sql [premieredYear ++ "-01-01" ] 

-- | Background method for 'countAllRunningProgram'.
--
-- Queries all programs that are running and outputs the result with type 'ShowWithDate'.
queryRunningProgram :: Connection -> IO [ShowWithDate]
queryRunningProgram conn = do
    let sql  = "SELECT DISTINCT program.id, program, ended FROM program, entries WHERE entries.fk_program == program.id  AND entries.status='Running' "
    query_ conn sql

-- | Takes the query generated from 'queryRunningProgram' and counts the numbers of rows produced.
countAllRunningProgram :: Connection -> IO ()
countAllRunningProgram conn = do
    runningPrograms <- queryRunningProgram conn
    let total = length runningPrograms
    print $ "The number of running programs in 2022 is : " ++ show(total)

countRecords :: Connection -> IO [Program]
countRecords conn = do
    let sql  = "SELECT DISTINCT * FROM program"
    query_ conn sql

-- | Takes the query generated from 'queryRunningProgram' and counts the numbers of rows produced.
countRecordsInt :: Connection -> IO Int
countRecordsInt conn = do
    count <- countRecords conn
    let total = length count
    return total


-- | Generate random number between 1 and 230 (the number of records in our database).
--
-- Makes use of the 'listToInt' function and System.Random import.
getRandomInt :: Connection -> IO Int
getRandomInt conn = do
   g <- getStdGen
   maxNoRecords <- countRecordsInt conn
   let num = listToInt (take 1 (randomRs (1, maxNoRecords) g :: [Int]))
   return num