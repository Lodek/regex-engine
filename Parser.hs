
import Data.Either

type Symbol = Char

data Operator = Concat | Alternation
data Quantifier = Kleene
data Token = SToken Symbol | QtToken Quantifier | OpToken Operator | GroupBegin | GroupEnd
data ParseTree = Node ParseTree Operator ParseTree | QuantifierLeaf ParseTree Quantifier | Leaf Symbol

getOperator :: Token -> Operator
getOperator (OpToken o) = o

getQuantifier :: Token -> Quantifier
getQuantifier (QtToken t) = t

getSymbol :: Token -> Symbol
getSymbol (SToken t) = t

buildQuantifiedLeaf :: Token -> Token -> ParseTree
buildQuantifiedLeaf s q = QuantifierLeaf $ Leaf $ getSymbol s getQuantifier q

genTokens :: String -> [Token]
genTokens  = map genToken

genToken :: Symbol -> Token
genToken c
    | c `elem` controlCharacters = controlToken c
    | c `elem` alphabet = SToken c
    | otherwise = error "Symbol " ++ [c] ++ "is not in alphabet."
    where controlCharacters = "()|*"
          alphabet = ['a'..'z']
          controlToken c = case c of '(' -> OpToken GroupBegin 
                                     ')' -> OpToken GroupEnd
                                     '|' -> OpToken Alternation
                                     '*' -> QtToken Kleene

-- |Adds explicit Concat tokens in between symbols or groups
normalizeStream :: [Token] -> [Token]
normalizeStream [] = []
normalizeStream (x1:[]) = [x1]
normalizeStream (x1:x2:xs) = case (x1, x2) of
                                  (SToken, SToken) -> x1:(OpToken Concat):x2:normalizeStream xs
                                  (SToken, GroupBegin) -> x1:(OpToken Concat):x2: normalizeStream xs
                                  (_, _) -> x1:x2: normalizeStream xs



-- |Classify and sort tokens into either operators (left) or trees (right).
-- Need to add suport for groups later on
sortAndTreefy :: [Token] -> [Either Operator ParseTree]
sortAndTreefy [] = []
sortAndTreefy ((s@SToken):(qt@QtToken):ts) = (Right $ buildQuantifiedLeaf s qt):sortAndTreefy ts
sortAndTreefy (t:ts) = case t of OpToken -> (Left $ getOperator t):(sortAndTreefy ts)
                                 SToken -> (Right $ Leaf t):(sortAndTreefy ts)


transformEithers :: [Either Operator ParseTree] -> ([Operator], [ParseTree])
transformEithers eithers = (lefts eithers, rights eithers)

mergeOps :: [Operator] -> [ParseTree] -> [ParseTree]
mergeOps [] (ts@t:[]) = ts
mergeOps (o:os) (ta:tb:ts) = mergeOps os (Node ta o tb):ts
