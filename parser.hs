type Symbol = Char 
data Operator = Concat | Alternation
data Quantifier = Kleene
data Token = Symbol | Quantifier | Operator | GroupBegin | GroupEnd
data ParseTree = Node ParseTree Operator ParseTree | QuantifierNode Symbol Quantifier | Singleton Symbol

symbols = "abcdefghjklmnopqrstuvwxyz"
alphabet = "()*|" ++ symbols

-- |Verify that all characters in the input string are part of the defined alphabet
isInAlphabet :: String -> Boolean
isInAlphabet = all $ map (\token -> token `elem` alphabet)

genTokens :: String -> [Token]
genTokens  = map genToken

genToken :: Symbol -> Token
genToken '(' = GroupBegin
	 ')' = GroupEnd
	 '|' = Alternation
	 '*' = Kleene
	 c = if c `elem` symbols then c else '!'


-- |Adds explicit Concat tokens in between symbols or groups
normalizeStream :: [Token] -> [Token]
normalizeStream [] = []
normalizeStream x1:[] = [x1]
normalizeStream x1:x2:xs = case (x1, x2) of
				(Symbol, Symbol) -> x1:Concat:x2: normalizeStream xs
				(Symbol, GroupBegin) -> x1:Concat:x2: normalizeStream xs
				(_, _) -> x1:x2: normalizeStream xs


treeBuilder :: [Token] -> (ParseTree, [Token])


seekTrees :: [Token] -> (ParseTree, [Token])
seekTrees x1:[] = x1
seekTrees x1:xs = case x1 of
                       Group_Begin -> treeBuilder xs
seekTrees x1:x2:xs = case (x1, x2) of
                          (Symbol, Quantifier) -> Node Symbol Quantifier
                          (Symbol, Operator) -> Node Symbol
