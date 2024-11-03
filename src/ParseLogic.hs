module ParseLogic where
import JSONGrammar
import ParseTreeBuilder

data ParseState = ParseState
  { symbolStack :: [Symbol]
  , treeStack :: [ParseTree]
  }

findRulesToApply :: Symbol -> Grammar -> [Rule]
findRulesToApply nt g = filter (\r -> lhs r == nt) (rules g)

startParse :: String -> Grammar -> Maybe ParseTree
startParse jstr grammar = iterOverStack startStacks jstr grammar
  where
    startRules = findRulesToApply (startSymbol grammar) grammar
    startStacks = map (\rule -> ParseState (rhs rule) [Node (lhs rule) []]) startRules


iterOverStack :: [ParseState] -> String -> Grammar -> Maybe ParseTree
iterOverStack [] _ _ = Nothing
iterOverStack ((ParseState [] [tree]):_) [] _ = Just tree  
iterOverStack ((ParseState [] _):states) input grammar = iterOverStack states input grammar
iterOverStack (state:states) input grammar =
    case stackChecker state grammar input of
        Just (remainingInput, newStates) ->
            -- if we're done
            if null remainingInput && any (\(ParseState syms _) -> null syms) newStates
            then Just (last . treeStack . head $ filter (\(ParseState syms _) -> null syms) newStates)
            -- or keep going
            else iterOverStack (newStates ++ states) remainingInput grammar
        -- dead state, kill
        Nothing ->
            iterOverStack states input grammar

stackChecker :: ParseState -> Grammar -> String -> Maybe (String, [ParseState])
stackChecker (ParseState (sym:syms) treeStack) grammar [] = Nothing
stackChecker (ParseState (sym:syms) treeStack) grammar input =
  case sym of
    NonTerminal nt ->
      Just (input, newStacks)
      where
        nextRules = findRulesToApply (NonTerminal nt) grammar
        newStacks = map (\rule -> ParseState
          (rhs rule ++ syms)
          (updateTreeStack (Node (lhs rule) []) treeStack))  
          nextRules
    Term t ->
      case matchTerminal t input of
        Just (matched, remainingInput) ->
          Just (remainingInput, [ParseState syms newTree])
          where
            newTree = updateTreeStack (Leaf matched) treeStack
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