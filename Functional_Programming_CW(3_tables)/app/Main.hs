module Main (main) where

import System.IO
import Types
import Fetch
import Parse
import Database

main :: IO ()
main = do
    putStrLn "---------------------------------"
    putStrLn "  Welcome to the Show data app   "
    putStrLn "  (1) Download data              "
    putStrLn "  (2) Play Guess the show        "
    putStrLn "  (3) Quit                       "
    putStrLn "---------------------------------"
    conn <- initialiseDB
    hSetBuffering stdout NoBuffering
    putStr "Choose an option > "
    option <- readLn :: IO Int
    case option of
        1 -> do
            let url = "https://api.tvmaze.com/shows"
            print "Downloading..."
            json <- download url
            print "Parsing..."
            case (parseRecords json) of
                Left err -> print err
                Right recs -> do
                    print "Saving on DB..."
                    saveRecordsEntries conn (recs)
                    saveRecordsSummary conn (recs)
                    print "Saved!"
                    main
        2 -> do
            entries <- queryProgramAllEntries conn
            mapM_ print entries
            putStrLn "Is this show 1"
            putStrLn "or 2"
            answerNum <- getRandomInt
            putStrLn "BEFORE"
            answerProg <- queryProgram conn
            putStrLn "AFTER"
            mapM_ print answerProg
            putStr "Choose an option > "
            choice <- readLn :: IO Int
            if choice == answerNum 
               then do 
                putStrLn "Correct!"
                main 
               else do 
                putStrLn "Wrong, better luck next time!"
                main
            -- case choice of
            --     answer -> do
            --         print answer
            --         putStrLn "Correct!"
            --         main
            --     2 -> do
            --         putStrLn "Wrong!"
            --         main
            --     _ -> do
            --         print "Invalid option"
            --         main
        3 -> print "Hope you've enjoyed using the app!"
        _ -> print "Invalid option"
