data Quantifier = Kleene | Optional
data Operation = Concat | Alternation
data Symbol = Char

alphabet = "abcdefghjklmnopqrstuvwxyz*()|"

data ParseTree = Node ParseTree Operation ParseTree | Node Parsetree Quantifier | Symbol

-- |Verify that all characters in the input string are part of the defined alphabet
isInAlphabet :: String -> Boolean
isInAlphabet = all $ map (\token -> token `elem` alphabet)

regexToTree :: String -> ParseTree
