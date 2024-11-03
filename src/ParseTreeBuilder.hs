module ParseTreeBuilder where
import JSONGrammar

data ParseTree = Node
  { nodeSymbol :: Symbol
  , children :: [ParseTree]
  } | Leaf Terminal
  deriving (Show)

-- connect the children
updateTreeStack :: ParseTree -> [ParseTree] -> [ParseTree]
updateTreeStack newNode [] = [newNode]
updateTreeStack newNode (Node sym children : rest) =
  Node sym (newNode : children) : rest