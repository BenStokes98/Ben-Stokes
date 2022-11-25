{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    getOrCreateProgram,
    saveRecords,
    queryProgramAllEntries
) where

import Types
import Database.SQLite.Simple

-- See more Database.SQLite.Simple examples at
-- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html

initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "program.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS program (id INTEGER PRIMARY KEY AUTOINCREMENT,program VARCHAR(80),site VARCHAR(300),status VARCHAR(80) DEFAULT NULL)"
        execute_ conn "CREATE TABLE IF NOT EXISTS entries (weight INT,summary VARCHAR(2000),runtime INT,premiered VARCHAR(40), ended INT DEFAULT NULL, language INT DEFAULT NULL, fk_program INTEGER)"
        return conn

getOrCreateProgram :: Connection -> Maybe String -> Maybe String -> Maybe String -> IO Program
getOrCreateProgram conn prog site stat = do
    results <- queryNamed conn "SELECT * FROM program WHERE program=:program AND status=:status" [":program" := prog, ":status" := stat]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO program (program, site, status) VALUES (?, ?, ?)" (prog, site, stat)
        getOrCreateProgram conn prog site stat

createRecord :: Connection -> Record -> IO ()
createRecord conn record = do
    c <- getOrCreateProgram conn (program record) (site record) (status record)
    let entry = Entry {
        weight_ = weight record :: Maybe Int,
        summary_ = summary record :: Maybe String,
        runtime_ = runtime record :: Maybe Int,
        premiered_ = premiered record :: Maybe String,
        ended_ = ended record :: Maybe String,
        language_ = language record :: Maybe String,
        fk_program = id_ c :: Maybe Int
    }
    execute conn "INSERT INTO entries VALUES (?,?,?,?,?,?,?)" entry

saveRecords :: Connection -> [Record] -> IO ()
saveRecords conn = mapM_ (createRecord conn)

queryProgramAllEntries :: Connection -> IO [Record]
queryProgramAllEntries conn = do
    putStr "Enter program name > "
    programID <- getLine
    putStrLn $ "Looking for " ++ programID ++ " entries..."
    let sql = "SELECT weight, summary, runtime, premiered, ended, language, program, site, status FROM entries inner join program on entries.fk_program == program.id where id = ?"
    query conn sql [programID]


