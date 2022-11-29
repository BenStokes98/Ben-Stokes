{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    getOrCreateProgram,
    createRecord,
    saveRecordsEntries,
    saveRecordsSummary,
    createSummary,
    getRandomInt,
    queryProgramAllEntries,
    queryProgram
) where

import Types
import Database.SQLite.Simple
import System.Random
-- See more Database.SQLite.Simple examples at
-- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html

initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "program.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS program (\
        \id INTEGER PRIMARY KEY AUTOINCREMENT,\
        \program VARCHAR(200) NOT NULL\
        \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS entries (\
        \weight INT,\
        \runtime INT,\
        \premiered VARCHAR(40), \
        \ended INT DEFAULT NULL,\
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

getOrCreateProgram :: Connection -> String -> IO Program
getOrCreateProgram conn prog = do
    results <- queryNamed conn "SELECT * FROM program WHERE program=:program" [":program" := prog]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO program (program) VALUES (?)" [prog :: String]
        getOrCreateProgram conn prog

-- getOrCreateSummary :: Connection -> Maybe String -> IO Program
-- getOrCreateSummary conn summ = do
--     results <- queryNamed conn "SELECT * FROM summary WHERE summary=:summary" [":program" := summ]    
--     if length results > 0 then
--         return . head $ results
--     else do
--         execute conn "INSERT INTO summary (summary) VALUES (?)" (summ)
--         getOrCreateProgram conn prog

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

createSummary :: Connection -> Record -> IO ()
createSummary conn record  = do
    c <- getOrCreateProgram conn (program record)
    let insertsummary = Summary {
        summary_ = summary record :: Maybe String,
        fk_program2 = id_ c :: Maybe Int
    }
    execute conn "INSERT INTO summary VALUES (?,?)" insertsummary


listToInt :: [Int] -> Int
listToInt [] = 0
listToInt (x:xs) = x

getRandomInt :: IO Int
getRandomInt = do
   g <- getStdGen
   let num = listToInt (take 1 (randomRs (1, 10) g :: [Int]))
   return num 

--in IO action: test <- getRandomInt

saveRecordsEntries :: Connection -> [Record] -> IO ()
saveRecordsEntries conn = mapM_ (createRecord conn)

saveRecordsSummary :: Connection -> [Record] -> IO ()
saveRecordsSummary conn = mapM_ (createSummary conn)

--"SELECT DISTINCT SUBSTR(summary, 4,LENGTH(summary)-6), fk_program FROM summary where summary.fk_program = ?"

queryProgramAllEntries :: Connection -> IO [Summary]
queryProgramAllEntries conn = do
    putStrLn "-----------------------------------------"
    putStrLn "Welcome to Guess the show"
    putStrLn "Here is the show description, good luck! "
    putStrLn "-----------------------------------------"
    test <- getRandomInt
    let sql = "SELECT DISTINCT REPLACE(REPLACE(REPLACE(summary, '<p>', ''),'</p>', ''),'<b>%</b>', ''), fk_program FROM summary where summary.fk_program = ?"
    query conn sql [test]


queryProgram :: Connection -> IO [oneProg]
queryProgram conn = do
    test <- getRandomInt
    let sql = "SELECT DISTINCT id, program FROM program where program.id = ?"
    query conn sql [test]

