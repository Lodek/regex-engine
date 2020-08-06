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

-- |Adds explicit Concat tokens in between symbols or groups
normalizeStream :: [Token] -> [Token]
normalizeStream [] = []
normalizeStream x1:[] = [x1]
normalizeStream x1:x2:xs = case (x1, x2) of
				(Symbol, Symbol) -> x1:Concat:x2: normalizeStream xs
				(Symbol, GroupBegin) -> x1:Concat:x2: normalizeStream xs
				(_, _) -> x1:x2: normalizeStream xs
