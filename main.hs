module Main where
import JSONGrammar
import ParseLogic
import ParseTreeBuilder

main :: IO ()
main = do
  let input = "{\"key\":\"value\"}"
  putStrLn $ "Parsing input: " ++ input
  case startParse input jsonGrammar of
    Just tree -> print tree
    Nothing -> putStrLn "Failed to parse"