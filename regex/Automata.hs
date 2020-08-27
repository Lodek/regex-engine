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


-- |Return list of states reachable from given states with epsilon transitions alone
epsilonClosure :: Delta s t -> [s] -> [s]
epsilonClosure delta [] = []
epsilonClosure delta ss = let ss' = concat $ map (\s -> delta s Epsilon) ss in
                              ss' ++ epsilonClosure ss'


stateEpsilonClosure :: Delta s t -> s -> [s]
stateEpsilonClosure delta s = epsilonClosure delta [s]


move :: Delta s t -> s -> AlphabetSymbol t -> [s]
move delta st sy = let ss = delta st sy in
                       ss:(epsilonClosure delta ss)


moveStates :: Delta s t -> [s] -> AlphabetSymbol t -> [s]
moveStates delta ss sy = concat $ map (\s -> move delta s sy) ss

--execNFA :: Automata t s -> [t] -> Bool
