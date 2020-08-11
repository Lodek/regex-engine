module Regex.Parser where

data Symbol = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'  deriving (Show, Read, Eq)
data ControlSymbol = '(' | ')' | '*' | '|' deriving (Show, Read, Eq)
data Alphabet = Symbol | ControlSymbol deriving (Show, Read, Eq)

data Operator = Concat | Alternation
data Quantifier = Kleene
data Token = Symbol | Quantifier | Operator | GroupBegin | GroupEnd
data ParseTree = Node ParseTree Operator ParseTree | QuantifierLeaf Symbol Quantifier | Leaf Symbol


genTokens :: String -> [Token]
genTokens  = map genToken

genToken :: Alphabet -> Token
genToken s@Symbol = s
genToken c@ControlSymbol = case c of '(' -> GroupBegin
                                     ')' -> GroupEnd
                                     '|' -> Alternation
                                     '*' -> Kleene
	 

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
