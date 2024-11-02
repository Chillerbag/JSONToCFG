module Main where

data Terminal = TermChar Char
  | TermInt Int
  | TermBool Bool
  | TermAnyChar Char
  | TermNull
  deriving (Show, Eq)

data Symbol = Term Terminal
  | NonTerminal Char
  deriving (Show, Eq)

data Rule = Rule
  { lhs :: Symbol
  , rhs :: [Symbol]
  }
  deriving (Show)

data Grammar = Grammar
  { rules :: [Rule]
  , startSymbol :: Symbol
  }
  deriving (Show)

data ParseTree = Node
  { nodeSymbol :: Symbol
  , children :: [ParseTree]
  } | Leaf Terminal
  deriving (Show)

jsonGrammar :: Grammar
jsonGrammar = Grammar
  { rules =
    [ Rule (NonTerminal 'O') [Term (TermChar '{'), NonTerminal 'M', Term (TermChar '}')]
    , Rule (NonTerminal 'O') [Term (TermChar '{'), Term (TermChar '}')] -- Most basic form of JSON 
    , Rule (NonTerminal 'M') [NonTerminal 'P'] -- this part describes key value pairs!
    , Rule (NonTerminal 'M') [NonTerminal 'P', Term (TermChar ','), NonTerminal 'M'] -- here we say that we have a sequnece of pairs 
    , Rule (NonTerminal 'P') [NonTerminal 'S', Term (TermChar ':'), NonTerminal 'V'] -- here we give form for KV pair 
    , Rule (NonTerminal 'A') [Term (TermChar '['), Term (TermChar ']')] -- list stuff
    , Rule (NonTerminal 'A') [Term (TermChar '['), NonTerminal 'E', Term (TermChar ']')] -- we should probably parse this as list properly, later.
    , Rule (NonTerminal 'E') [NonTerminal 'V'] -- values in a list 
    , Rule (NonTerminal 'E') [NonTerminal 'V', Term (TermChar ','), NonTerminal 'E']
    , Rule (NonTerminal 'V') [NonTerminal 'S'] -- value that a key can hold. Strings, Numbers, JSON, Lists. 
    , Rule (NonTerminal 'V') [NonTerminal 'N']
    , Rule (NonTerminal 'V') [NonTerminal 'O']
    , Rule (NonTerminal 'V') [NonTerminal 'A']
    , Rule (NonTerminal 'V') [Term (TermBool True)] -- bools 
    , Rule (NonTerminal 'V') [Term (TermBool False)]
    , Rule (NonTerminal 'V') [Term TermNull]
    , Rule (NonTerminal 'S') [Term (TermChar '"'), NonTerminal 'H', Term (TermChar '"')] -- here we start dealing with strings 
    , Rule (NonTerminal 'S') [Term (TermChar '"'), Term (TermChar '"')]
    , Rule (NonTerminal 'H') [Term (TermAnyChar 'x')]
    , Rule (NonTerminal 'H') [Term (TermAnyChar 'x'), NonTerminal 'H']
    , Rule (NonTerminal 'N') [NonTerminal 'I'] -- ok here we start building numbers
    , Rule (NonTerminal 'N') [NonTerminal 'I', NonTerminal 'F']
    , Rule (NonTerminal 'N') [NonTerminal 'I', NonTerminal 'X']
    , Rule (NonTerminal 'N') [NonTerminal 'I', NonTerminal 'F', NonTerminal 'X']
    , Rule (NonTerminal 'I') [NonTerminal 'G']
    , Rule (NonTerminal 'I') [Term (TermChar '-'), NonTerminal 'G'] -- come back to this
    , Rule (NonTerminal 'F') [Term (TermChar '.'),  NonTerminal 'G']
    , Rule (NonTerminal 'X') [NonTerminal 'B', NonTerminal 'G'] -- this rule is for scientific notation. 
    , Rule (NonTerminal 'G') [NonTerminal 'D'] -- numbers
    , Rule (NonTerminal 'G') [NonTerminal 'D', NonTerminal 'G']
    , Rule (NonTerminal 'B') [Term (TermChar 'e')]
    , Rule (NonTerminal 'B') [Term (TermChar 'e'), Term (TermChar '+')]
    , Rule (NonTerminal 'B') [Term (TermChar 'e'), Term (TermChar '-')]
    , Rule (NonTerminal 'D') [Term (TermInt 0)] -- every integer we can use
    , Rule (NonTerminal 'D') [Term (TermInt 1)]
    , Rule (NonTerminal 'D') [Term (TermInt 2)]
    , Rule (NonTerminal 'D') [Term (TermInt 3)]
    , Rule (NonTerminal 'D') [Term (TermInt 4)]
    , Rule (NonTerminal 'D') [Term (TermInt 5)]
    , Rule (NonTerminal 'D') [Term (TermInt 6)]
    , Rule (NonTerminal 'D') [Term (TermInt 7)]
    , Rule (NonTerminal 'D') [Term (TermInt 8)]
    , Rule (NonTerminal 'D') [Term (TermInt 9)]
    ]
    ,
    startSymbol = NonTerminal 'O'
  }

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

