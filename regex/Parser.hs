module Parser where 
import Data.Either

type Symbol = Char

data Operator = Concat | Alternation deriving (Show, Eq)
data Quantifier = Kleene deriving (Show, Eq)
data Token = SToken Symbol | QtToken Quantifier | OpToken Operator | GroupBegin | GroupEnd deriving (Show, Eq)

data ParseTree = Node ParseTree Operator ParseTree | QuantifierLeaf ParseTree Quantifier | Leaf Symbol deriving (Show, Eq)

genTokens :: [Symbol] -> [Token]
genTokens  = map genToken

genToken :: Symbol -> Token
genToken c
    | c `elem` controlCharacters = controlToken c
    | c `elem` alphabet = SToken c
    | otherwise = error $ "Symbol " ++ [c] ++ "is not in alphabet."
    where controlCharacters = "()|*"
          alphabet = ['a'..'z']
          controlToken c = case c of '(' -> GroupBegin 
                                     ')' -> GroupEnd
                                     '|' -> OpToken Alternation
                                     '*' -> QtToken Kleene


-- |Adds explicit Concat tokens in between symbols or groups
normalizeStream :: [Token] -> [Token]
normalizeStream [] = []
normalizeStream (x1:[]) = [x1]
normalizeStream (x1:x2:xs) = case (x1, x2) of
                                  (SToken _, SToken _) -> x1:(OpToken Concat):normalizeStream (x2:xs)
                                  (SToken _, GroupBegin) -> x1:(OpToken Concat): normalizeStream (x2:xs)
                                  (_, _) -> x1:normalizeStream (x2:xs)

evenGroupPredicate :: [Token] -> Bool
evenGroupPredicate ts = let begins = length . filter (\t -> t == GroupBegin) 
                            ends = length . filter (\t -> t == GroupEnd) in
                        begins ts == ends ts

uniqueQuantifierPredicate :: [Token] -> Bool
uniqueQuantifierPredicate [] = True
uniqueQuantifierPredicate ((QtToken _):(QtToken _):ts) = False
uniqueQuantifierPredicate (t:ts) = True && uniqueQuantifierPredicate ts

validateTokens :: [Token] -> Bool
validateTokens ts = and ( predicates <*> pure ts) -- Applicative functors
   where predicates = [evenGroupPredicate, uniqueQuantifierPredicate]

validateAndReturn :: [Token] -> [Token]
validateAndReturn ts = let valid = validateTokens ts in
                       case valid of
                            True -> ts
                            False -> error "Invalid input"

-- |Classify and sort tokens into either operators (left) or trees (right).
-- Need to add suport for groups later on
sortAndTreefy :: [Token] -> [Either Operator ParseTree]
sortAndTreefy [] = []
sortAndTreefy ((SToken s):(QtToken q):ts) = (Right $ QuantifierLeaf (Leaf s) q):sortAndTreefy ts
sortAndTreefy (t:ts) = case t of (OpToken t) -> (Left t):(sortAndTreefy ts)
                                 (SToken t) -> (Right $ Leaf t):(sortAndTreefy ts)


transformEithers :: [Either Operator ParseTree] -> ([Operator], [ParseTree])
transformEithers eithers = (lefts eithers, rights eithers)

mergeOps :: ([Operator], [ParseTree]) -> [ParseTree]
mergeOps ([], []) = []
mergeOps ((o:os), (ta:tb:ts)) = (Node ta o tb):mergeOps (os, ts)

buildTree :: String -> ParseTree
buildTree = head . mergeOps . transformEithers . sortAndTreefy . normalizeStream . validateAndReturn . genTokens
