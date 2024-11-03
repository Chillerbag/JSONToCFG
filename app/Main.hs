module Main where

import JSONGrammar
import ParseLogic

main :: IO ()
main = do
  putStrLn "Please enter JSON as a string or 'quit' to quit."
  input <- getLine
  case input of
    "quit" -> putStrLn "Thanks for using the JSON to CFG parser!"
    _ -> do 
      case startParse input jsonGrammar of
        Just tree -> do
          putStrLn "\nParsed JSON as a parse tree!\n"
          print tree
        Nothing -> putStrLn "\nFailed to parse"
      main