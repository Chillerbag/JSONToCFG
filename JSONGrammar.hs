module JSONGrammar where
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