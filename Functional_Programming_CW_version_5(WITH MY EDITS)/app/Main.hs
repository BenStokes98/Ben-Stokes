{-|
Module      : Main
Copyright   : (c) Ben Stokes, 2022
                  Navid Sheikh, 2022
-}


module Main (main) where

import System.IO
import Fetch
import Parse
import Database

-- | Display menu to allow the user to execute a command.
main :: IO ()
main = do
    putStrLn "---------------------------------------------------------"
    putStrLn "  Welcome to TV SERIES program                           "
    putStrLn "  (1) Download data                                      "
    putStrLn "  (2) Display shows                                      "
    putStrLn "  (3) Show summary of a show (ID)                        "
    putStrLn "  (4) Show all programs that have ENDED                  "
    putStrLn "  (5) Show all movies premiered before a certain YEAR    "
    putStrLn "  (6) Guess the shows - GAME                             "
    putStrLn "  (7) Count the number of shows that are still RUNNING   "
    putStrLn "  (8) Quit                                               "
    putStrLn "---------------------------------------------------------"
    conn <- initialiseDB
    hSetBuffering stdout NoBuffering
    putStr "Choose an option > "
    option <- readLn :: IO Int
    case option of
        1 -> do
            let url = "https://api.tvmaze.com/shows"
            print "Downloading data..."
            json <- download url
            case (parseRecords json) of
                Left err -> print err
                Right recs -> do
                    print "Saving onto the database..."
                    saveRecordsEntries conn (recs)
                    saveRecordsSummary conn (recs)
                    print "Saved!"
                    main
        2 -> do
            entries <- queryProgramAllEntries conn
            mapM_ print entries
            main
        3 -> do
            entries <- queryProgramSummary conn
            mapM_ print entries
            main
        4 -> do
            entries <- queryAllProgramEnded conn
            mapM_ print entries
            main
        5 -> do
            entries <- queryProgramPremieredDate conn
            mapM_ print entries
            main
        6 -> do
            answerNum <- getRandomInt conn 
            putStrLn "Welcome to GUESS  show"
            randomSummary <- queryProgramRandomSummary conn answerNum
            mapM_ print randomSummary
            entries <- query5RandomPrograms conn answerNum
            mapM_ print entries
            putStrLn "-----------------------------------------"
            putStr "Guess the ID show based on the description above > "
            choice <- readLn :: IO Int
            if choice == answerNum
                then do 
                putStrLn "Correct!"
                main 
               else do 
                putStrLn  $ "The answer was " ++ (show(answerNum))
                putStrLn "Wrong, better luck next time!"
                main
        7 -> do
            countAllRunningProgram conn
            main
        8 -> print "Hope you've enjoyed using the app!"
        _ -> print "!!ERROR!!  Invalid option. Please try again.  !!ERROR!! "
