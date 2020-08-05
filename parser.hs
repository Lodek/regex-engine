data Symbol = Char
data Token = Symbol | Kleene | Alternation | Concat | GroupBegin | GroupEnd

data ParseTree = Node ParseTree Operation ParseTree | Node Parsetree Quantifier | Symbol

symbols = "abcdefghjklmnopqrstuvwxyz"
alphabet = "()*|" ++ symbols

-- |Verify that all characters in the input string are part of the defined alphabet
isInAlphabet :: String -> Boolean
isInAlphabet = all $ map (\token -> token `elem` alphabet)

regexToTree :: String -> ParseTree

genTokens :: String -> [Token]
genTokens  = map genToken

genToken :: Symbol -> Token
genToken '(' = GroupBegin
	 ')' = GroupEnd
	 '|' = Alternation
	 '*' = Kleene
	 c = if c `elem` symbols then c else '!'

genTree :: [Token] -> ParseTree
