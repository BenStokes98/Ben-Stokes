{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    getOrCreateCountry,
    saveRecords,
    queryCountryAllEntries,
    queryCountryTotalCases
) where

import Types
import Database.SQLite.Simple

-- See more Database.SQLite.Simple examples at
-- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html

initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "covid.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS shows (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \title VARCHAR(80) NOT NULL, \
            \genre VARCHAR(50) NOT NULL, \
            \status VARCHAR(30) NOT NULL, \
            \fullname VARCHAR(30) NOT NULL, \
            \rating INT DEFAULT NULL \
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS actors (\
            \fullname VARCHAR(40) NOT NULL, \
            \gender VARCHAR(40) NOT NULL, \
            \birthday VARCHAR(40) NOT NULL, \
            \fk_shows VARCHAR(40)\
            \)"
        return conn

getOrCreateCountry :: Connection -> String -> String -> Maybe Int -> IO Country
getOrCreateCountry conn coun cont pop = do
    results <- queryNamed conn "SELECT * FROM countries WHERE country=:country AND continent=:continent" [":country" := coun, ":continent" := cont]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO countries (country, continent, population) VALUES (?, ?, ?)" (coun, cont, pop)
        getOrCreateCountry conn coun cont pop

createRecord :: Connection -> Record -> IO ()
createRecord conn record = do
    c <- getOrCreateCountry conn (country record) (continent record) (population record)
    let entry = Entry {
        date_ = date record,
        day_ = day record,
        month_ = month record,
        year_ = year record,
        cases_ = cases record,
        deaths_ = deaths record,
        fk_country = id_ c
    }
    execute conn "INSERT INTO entries VALUES (?,?,?,?,?,?,?)" entry

saveRecords :: Connection -> [Record] -> IO ()
saveRecords conn = mapM_ (createRecord conn)

queryCountryAllEntries :: Connection -> IO [Record]
queryCountryAllEntries conn = do
    putStr "Enter country name > "
    countryName <- getLine
    putStrLn $ "Looking for " ++ countryName ++ " entries..."
    let sql = "SELECT date, day, month, year, cases, deaths, country, continent, population FROM entries inner join countries on entries.fk_country == countries.id WHERE country=?"
    query conn sql [countryName]

queryCountryTotalCases :: Connection -> IO ()
queryCountryTotalCases conn = do
    countryEntries <- queryCountryAllEntries conn
    let total = sum (map cases countryEntries)
    print $ "Total entries: " ++ show(total)
