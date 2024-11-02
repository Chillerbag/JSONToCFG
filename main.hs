module Main where
import JSONGrammar

data ParseTree = Node
  { nodeSymbol :: Symbol
  , children :: [ParseTree]
  } | Leaf Terminal
  deriving (Show)

findRulesToApply :: Symbol -> Grammar -> [Rule]
findRulesToApply nt g = filter (\r -> lhs r == nt) (rules g)

startParse :: String -> Grammar -> Maybe ParseTree
startParse jstr grammar = iterOverStack startStacks jstr grammar
  where
    startStacks = map rhs (findRulesToApply (startSymbol grammar) grammar)

iterOverStack :: [[Symbol]] -> String -> Grammar -> Maybe ParseTree
iterOverStack [] [] _ = Nothing
iterOverStack (stacksH:stacksT) (jsonHead:jsonTail) grammar =
  case stackChecker stacksH grammar (jsonHead:jsonTail)  of
    Just (remainingInput, moreStacks) -> iterOverStack (moreStacks ++ stacksT) remainingInput grammar
    Nothing -> iterOverStack stacksT (jsonHead:jsonTail) grammar -- prune dead stacks. 

stackChecker :: [Symbol] -> Grammar -> String -> Maybe (String, [[Symbol]])
stackChecker (sym:syms) grammar input =
  case sym of
    -- in this case, we havent moved, just added new things to check
    NonTerminal nt -> Just (input, newStacks)
      where
        nextRules = findRulesToApply (NonTerminal nt) grammar
        nextSyms = map rhs nextRules
        newStacks = map (++ syms) nextSyms
    Term t ->
      case matchTerminal t input of
        Just (_, remainingInput) -> Just (remainingInput, [syms])  -- Continue with remaining symbols
        Nothing -> Nothing

-- check if something is the same as its terminal (first we must parse to type)
matchTerminal :: Terminal -> String -> Maybe (Terminal, String)
matchTerminal _ [] = Nothing

matchTerminal (TermChar char) (cur:rest) =
  if char == cur
  then Just (TermChar cur, rest)
  else Nothing

matchTerminal (TermBool _) input
  | take 4 input == "True" = Just (TermBool True, drop 4 input)
  | take 5 input == "false" = Just (TermBool False, drop 5 input)
  | otherwise = Nothing

matchTerminal TermNull input
  | take 4 input == "null" = Just (TermNull, drop 4 input)
  | otherwise = Nothing

matchTerminal (TermInt _) input =
  case reads input of
    [(num, rest)] -> Just (TermInt num, rest)
    _ -> Nothing

matchTerminal (TermAnyChar _) (cur:rest) =
  if isValidJSONChar cur
  then Just (TermAnyChar cur, rest)
  else Nothing

isValidJSONChar :: Char -> Bool
isValidJSONChar c =
  c /= '"' && c /= '\\' && c >= '\x20'

main :: IO ()
main = print (startParse "{\"key\":\"\"}" jsonGrammar)

