{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    getOrCreatePrograms,
    saveRecords,
    queryProgramsAllActors,
    queryProgramsTotalCases
) where

import Types
import Database.SQLite.Simple

-- See more Database.SQLite.Simple examples at
-- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html

initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "program.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS programs (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \program VARCHAR(80) NOT NULL, \
            \genre VARCHAR(50) NOT NULL, \
            \status VARCHAR(30) NOT NULL, \
            \rating INT DEFAULT NULL \
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS actors (\
            \fullname VARCHAR(40) NOT NULL, \
            \gender VARCHAR(40) NOT NULL, \
            \birthday VARCHAR(40) NOT NULL, \
            \fk_program INTEGER\
            \)"
        return conn

getOrCreatePrograms :: Connection -> String -> String -> Maybe Int -> IO Programs
getOrCreatePrograms conn coun cont pop = do
    results <- queryNamed conn "SELECT * FROM programs WHERE program=:program" [":program" := program]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO programs (program, genre, status) VALUES (?, ?, ?, ?)" (program, genr, stat, rate)
        getOrCreatePrograms conn program genr stat rate

createRecord :: Connection -> Record -> IO ()
createRecord conn record = do
    c <- getOrCreatePrograms conn (program record) (genr record) (stat record) (rate record)
    let actors = Actors {
        fullname_ = fullname record,
        gender_ = gender record,
        birthday_ = birthday record,
        fk_program = id_ c
    }
    execute conn "INSERT INTO actors VALUES (?,?,?,?)" actors

saveRecords :: Connection -> [Record] -> IO ()
saveRecords conn = mapM_ (createRecord conn)

queryProgramsAllActors :: Connection -> IO [Record]
queryProgramsAllActors conn = do
    putStr "Enter program name > "
    programName <- getLine
    putStrLn $ "Looking for " ++ programName ++ " entries..."
    let sql = "SELECT actors.fullname, actors.gender, actors.birthday, programs.program, programs.genre, programs.status, programs.rating  FROM actors inner join programs on actors.fk_program == programs.id WHERE program=?"
    query conn sql [programName]

--queryProgramsTotalCases :: Connection -> IO ()
--queryProgramsTotalCases conn = do
--    showActors <- queryProgramsAllActors conn
 --   let total = sum (map cases showActors)
   -- print $ "Total actors: " ++ show(total)
