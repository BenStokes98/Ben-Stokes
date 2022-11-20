module Main (main) where

import System.IO
import Types
import Fetch
import Parse
import Database

main :: IO ()
main = do
    putStrLn "---------------------------------"
    putStrLn "  Welcome to the Covid data app  "
    putStrLn "  (1) Download data              "
    putStrLn "  (2) All entries by program       "
    putStrLn "  (3) Total cases by program        "
    putStrLn "  (4) Quit                       "
    putStrLn "---------------------------------"
    conn <- initialiseDB
    hSetBuffering stdout NoBuffering
    putStr "Choose an option > "
    option <- readLn :: IO Int
    case option of
        1 -> do
            let url = "https://api.tvmaze.com/shows/1"
            print "Downloading..."
            json <- download url
            print "Parsing..."
            case (parseRecords json) of
                Left err -> print err
                Right recs -> do
                    print "Saving on DB..."
                    saveRecords conn (records recs)
                    print "Saved!"
                    main
        2 -> do
            actors <- queryProgramsAllActors conn
            mapM_ print actors
            main
        3 -> do
            queryProgramsTotalCases conn
            main
        4 -> print "Hope you've enjoyed using the app!"
        _ -> print "Invalid option"
