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
    putStrLn "  (2) Program ID    "
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
                    saveRecords conn (recs)
                    print "Saved!"
                    main
        2 -> do
            entries <- queryProgramAllEntries conn
            mapM_ print entries
            main
        3 -> print "Hope you've enjoyed using the app!"
        _ -> print "Invalid option"
