module Main where 

data Terminal = TermString String 
  | TermInt Int
  | TermBool Bool
  deriving (Show, Eq)


data Symbol = Term Terminal 
  | NonTerminal String 
  deriving (Show, Eq)

data Rule = Rule 
  { lhs :: Symbol
  , rhs :: [Symbol]
  } deriving (Show)

data Grammar = Grammar
  { rules :: [Rule]
  , startSymbol :: Symbol
  } deriving (Show)

jsonGrammar :: Grammar
jsonGrammar = Grammar 
  { rules = 
    [ Rule (NonTerminal "O") [Term (TermString "{"), NonTerminal "M", Term (TermString "}")]
    -- Most basic form of JSON 
    , Rule (NonTerminal "O") [Term (TermString "{"), Term (TermString "}")]

    -- this part describes key value pairs!
    , Rule (NonTerminal "M") [NonTerminal "P"]
    -- here we say that we have a sequnece of pairs 
    , Rule (NonTerminal "M") [NonTerminal "P", Term (TermString ","), NonTerminal "M"]
    -- here we give form for KV pair 
    , Rule (NonTerminal "P") [NonTerminal "S", Term (TermString ":"), NonTerminal "V"]

    -- list stuff 
    -- we should probably parse this as list properly, later.
    , Rule (NonTerminal "A") [Term (TermString "[]")]
    , Rule (NonTerminal "A") [Term (TermString "["), NonTerminal "E", Term (TermString "]")]
    -- values in a list 
    , Rule (NonTerminal "E") [NonTerminal "V"]
    , Rule (NonTerminal "E") [NonTerminal "V", Term (TermString ","), NonTerminal "E"]

    -- value that a key can hold. Strings, Numbers, JSON, Lists. 
    , Rule (NonTerminal "V") [NonTerminal "S"]
    , Rule (NonTerminal "V") [NonTerminal "N"]
    , Rule (NonTerminal "V") [NonTerminal "O"]
    , Rule (NonTerminal "V") [NonTerminal "A"]
    , Rule (NonTerminal "V") [Term (TermBool True)]
    , Rule (NonTerminal "V") [Term (TermBool False)]
    , Rule (NonTerminal "V") [Term (TermString "null")] -- TODO: LOOK INTO HOW WE DEAL WITH PARSING NULL

    -- here we start dealing with strings 
    , Rule (NonTerminal "S") [Term (TermString "\"" ), Term (TermString "\"")]
    , Rule (NonTerminal "S") [Term (TermString "\""), NonTerminal "H", Term (TermString "\"")]
    , Rule (NonTerminal "H") [NonTerminal "H"]
    , Rule (NonTerminal "H") [NonTerminal "C", NonTerminal "H"]
    , Rule (NonTerminal "C") [NonTerminal ""]-- uhhh

    -- ok here we start building numbers
    , Rule (NonTerminal "N") [NonTerminal "I"]
    , Rule (NonTerminal "N") [NonTerminal "I", NonTerminal "F"]
    , Rule (NonTerminal "N") [NonTerminal "I", NonTerminal "X"]
    , Rule (NonTerminal "N") [NonTerminal "I", NonTerminal "F", NonTerminal "X"]
    , Rule (NonTerminal "I") [NonTerminal "G"]
    , Rule (NonTerminal "I") [Term (TermString "-"), NonTerminal "G"] -- maybe make a terminal type that takes funcs ? 
    , Rule (NonTerminal "F") [Term (TermString "."),  NonTerminal "G"]
    -- this rule is for scientific notation. 
    , Rule (NonTerminal "X") [NonTerminal "B", NonTerminal "G"]
    -- numbers
    , Rule (NonTerminal "G") [NonTerminal "D"]
    , Rule (NonTerminal "G") [NonTerminal "D", NonTerminal "G"]
    , Rule (NonTerminal "B") [Term (TermString "e")] --TODO ok this sucks figure this out proper 

    , Rule (NonTerminal "B") [Term (TermString "e"), Term (TermString "+")] -- power? 
    , Rule (NonTerminal "B") [Term (TermString "e"), Term (TermString "-")]
    -- every integer 
    , Rule (NonTerminal "D") [Term (TermInt 0)]
    , Rule (NonTerminal "D") [Term (TermInt 1)]
    , Rule (NonTerminal "D") [Term (TermInt 2)]
    , Rule (NonTerminal "D") [Term (TermInt 3)]
    , Rule (NonTerminal "D") [Term (TermInt 4)]
    , Rule (NonTerminal "D") [Term (TermInt 5)]
    , Rule (NonTerminal "D") [Term (TermInt 6)]
    , Rule (NonTerminal "D") [Term (TermInt 7)]
    , Rule (NonTerminal "D") [Term (TermInt 8)]
    , Rule (NonTerminal "D") [Term (TermInt 9)]
    ]   
    ,
    startSymbol = NonTerminal "O" 
  }

main :: IO () 
main = putStrLn "Hello"
