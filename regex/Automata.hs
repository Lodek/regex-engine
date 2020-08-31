module Automata where 

import qualified Data.Set as Set
import Data.Either

data SigmaElem s = Symbol s | Epsilon deriving (Show, Eq, Ord)

type Sigma s = Set.Set (SigmaElem s)

type Delta q s = (q -> SigmaElem s -> [q])

data Automata q s = NFA { alphabet :: Set.Set (SigmaElem s),
                          states :: Set.Set q,
                          q0 :: q,
                          qa :: Set.Set q,
                          delta :: Delta q s
                        } 



-- |Wrap foldable of symbols as a list of SigmaElems, adding epsilon
buildSigma :: (Foldable t, Ord a) => t a -> Set.Set (SigmaElem a)
buildSigma ts = Set.insert Epsilon symbols
    where empty = Set.empty :: Set.Set (SigmaElem a)
          symbols = foldl (\acc s -> Set.insert (Symbol s) acc) empty ts

-- |If state is in return true if state is accepting else false
--isAccepting :: [q] -> q -> Bool
--isAccepting qs q = q elem qs



-- |Return list of states reachable from given states with epsilon transitions alone
--epsilonClosure :: Delta s t -> [s] -> [s]
--epsilonClosure delta [] = []
--epsilonClosure delta ss = let ss' = concat $ map (\s -> delta s Epsilon) ss in
                              --ss' ++ epsilonClosure delta ss'


--stateEpsilonClosure :: Delta s t -> s -> [s]
--stateEpsilonClosure delta s = epsilonClosure delta [s]


--move :: Delta s t -> s -> SigmaElem t -> [s]
--move delta st sy = let ss = delta st sy in
                       --ss ++ epsilonClosure delta ss


--moveStates :: Delta s t -> [s] -> SigmaElem t -> [s]
--moveStates delta ss sy = concat $ map (\s -> move delta s sy) ss


-- |Validate elements of stream are in Sigma, return either with error message or stream
--streamInSigma :: Sigma s -> [s] -> Either [String] [s]

--eval :: Automata q s -> [s] -> Either String Bool
--simple functions and  delegate error handling
--have wrapper functions to operate over automata
--dumb implementation, no partial matching; valid only whole stream is valid.
--public interface for building automata and state transition function
