import qualified Data.Set as Set

type Delta s t = (s -> AlphabetSymbol t -> [s])

data AlphabetSymbol t = Symbol t | Epsilon

data Automata t s = NFA { alphabet :: Set AlphabetSymbol t,
                          states :: Set s,
                          q0 :: s,
                          qa :: Set s,
                          delta :: (s -> t -> Set s)
                        } Deriving (Show)


-- |Build alphabet with Epsilon from list of symbols
buildEpsilonAlphabet :: [t] -> Set AlphabetSymbol t
buildEpsilonAlphabet ts = Set.fromList $ Epsilon:(map Symbol ts)


--stateEpsilonClosure :: Delta s t -> s -> [s]
--setEpsilonClosure :: Delta s t -> [s] -> [s]
--move :: Delta s t -> [s] -> [s]
--execNFA :: Automata t s -> [t] -> Bool
