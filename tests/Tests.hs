module Main where

import JSONGrammar
import ParseLogic
import ParseTreeBuilder
import System.FilePath ((</>))

main :: IO ()
main = do
    let files = map ("tests" </>) ["test1.json", "test2.json", "test3.json"]
    mapM_ parseFile files
  where
    parseFile file = do
        putStrLn $ "\nParsing " ++ file ++ ":"
        contents <- readFile file
        -- Print the raw contents to see what we're getting
        putStrLn "Raw contents:"
        print contents
        -- Try parsing
        case startParse contents jsonGrammar of
            Just tree -> do
                putStrLn "\nParsed JSON as a parse tree!\n"
                print tree
            Nothing -> do
                putStrLn "Failed to parse"
                -- Print the contents character by character to debug
                putStrLn "Content characters:"
                mapM_ (\c -> putStr $ show c ++ " ") contents
                putStrLn ""