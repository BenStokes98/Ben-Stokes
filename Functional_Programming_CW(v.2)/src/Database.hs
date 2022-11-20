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
        execute_ conn "CREATE TABLE IF NOT EXISTS program (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \program VARCHAR(80) NOT NULL, \
            \genres VARCHAR(300) NOT NULL, \
            \status VARCHAR(80) DEFAULT NULL \
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS entries (\
            \rating FLOAT NOT NULL, \
            \summary VARCHAR(2000) NOT NULL, \
            \runtime INT NOT NULL, \
            \premiered VARCHAR(40) NOT NULL, \
            \ended INT DEFAULT NULL, \
            \language INT DEFAULT NULL, \
            \fk_program INTEGER\
            \)"
        return conn

getOrCreateProgram :: Connection -> String -> String -> String -> IO Program
getOrCreateProgram conn prog genr stat = do
    results <- queryNamed conn "SELECT * FROM program WHERE program=:program AND status=:status" [":program" := prog, ":status" := stat]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO program (program, genr, status) VALUES (?, ?, ?)" (prog, genr, stat)
        getOrCreateProgram conn prog genr stat

createRecord :: Connection -> Record -> IO ()
createRecord conn record = do
    c <- getOrCreateProgram conn (program record) (genres record) (status record)
    let entry = Entry {
        rating_ = rating record,
        summary_ = summary record,
        runtime_ = runtime record,
        premiered_ = premiered record,
        ended_ = ended record,
        language_ = language record,
        fk_program = id_ c
    }
    execute conn "INSERT INTO entries VALUES (?,?,?,?,?,?,?)" entry

saveRecords :: Connection -> [Record] -> IO ()
saveRecords conn = mapM_ (createRecord conn)

queryProgramAllEntries :: Connection -> IO [Record]
queryProgramAllEntries conn = do
    putStr "Enter program name > "
    programName <- getLine
    putStrLn $ "Looking for " ++ programName ++ " entries..."
    let sql = "SELECT rating, summary, runtime, premiered, ended, language, program, genres, status FROM entries inner join program on entries.fk_program == program.id WHERE program=?"
    query conn sql [programName]


