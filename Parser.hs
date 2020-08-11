module Parser where

type Symbol = Char

data Operator = Concat | Alternation
data Quantifier = Kleene
data Token = Symbol | Quantifier | Operator | GroupBegin | GroupEnd
data ParseTree = Node ParseTree Operator ParseTree | QuantifierLeaf Symbol Quantifier | Leaf Symbol

genTokens :: String -> [Token]
genTokens  = map genToken

genToken :: Symbol -> Token
genToken c
    | c `elem` controlCharacters = controlToken c
    | c `elem` alphabet = c
    | otherwise = error "Symbol " ++ [c] ++ "is not in alphabet."
    where controlCharacters = "()|*"
          alphabet = [a..z]
          controlToken c = case c of '(' -> GroupBegin
                                     ')' -> GroupEnd
                                     '|' -> Alternation
                                     '*' -> Kleene

-- |Adds explicit Concat tokens in between symbols or groups
normalizeStream :: [Token] -> [Token]
normalizeStream [] = []
normalizeStream (x1:[]) = [x1]
normalizeStream (x1:x2:xs) = case (x1, x2) of
                                  (Symbol, Symbol) -> x1:Concat:x2: normalizeStream xs
                                  (Symbol, GroupBegin) -> x1:Concat:x2: normalizeStream xs
                                  (_, _) -> x1:x2: normalizeStream xs



-- |Classify and sort tokens into either operators (left) or trees (right).
-- Need to add suport for groups later on
sortAndTreefy :: [Token] -> [Either Operator ParseTree]
sortAndTreefy [] trees ops = []
sortAndTreefy ((symbol@Symbol):(quantifier@Quantifier):ts) = (Right $ QuantifierLeaf symbol quantifier):sortAndTreefy ts
sortAndTreefy (t:ts) = case t of Operator -> Left t : sortAndTreefy ts
                                 Symbol -> Right $ Leaf t : sortAndTreefy ts


transformEithers :: [Either Operator ParseTree] -> ([Operator], [ParseTree])
transformEithers eithers = (lefts eithers, right eithers)

mergeOps :: [Operator] -> [ParseTree] -> [ParseTree]
mergeOps [] (ts@t:[]) = ts
mergeOps (o:os) (ta:tb:ts) = mergeOps os (Node ta o tb):ts
