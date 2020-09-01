module Automata where 

import qualified Data.Set as Set
import Data.Either
import Data.Tuple

data SigmaElem s = Symbol s | Epsilon deriving (Show, Eq)

instance (Ord s) => Ord (SigmaElem s) where --unecessary, just for learning
    Epsilon < _ = True
    _ < Epsilon = False
    Symbol a < Symbol b = a < b

    a <= b = a < b || a == b


type Sigma s = Set.Set (SigmaElem s)

type Delta q s = (q -> SigmaElem s -> Set.Set q)

--data Automata q s = NFA { alphabet :: Set.Set (SigmaElem s),
                          --states :: Set.Set q,
                          --q0 :: q,
                          --qas :: Set.Set q,
                          --delta :: Delta q s
                        --} 


-- |Wrap foldable of symbols as a list of SigmaElems, adding epsilon
buildSigma :: (Foldable t, Ord a) => t a -> Set.Set (SigmaElem a)
buildSigma ts = Set.insert Epsilon symbols
    where empty = Set.empty :: Set.Set (SigmaElem a)
          symbols = foldl (\acc s -> Set.insert (Symbol s) acc) empty ts

-- | Unites the two sets specified as argument, apply function to first - second
-- | return pair with union and result of function application over the difference.
unionAndApplyOverDiff :: (Ord a) => (Set.Set a -> Set.Set a) -> Set.Set a -> Set.Set a -> (Set.Set a,  Set.Set a)
unionAndApplyOverDiff f a b = (Set.union a b, f (Set.difference a b))

-- | Apply delta over Set of states for a symbol, return the union of all reachable states
deltaOverSet :: (Ord q) => Delta q s -> SigmaElem s -> Set.Set q -> Set.Set q
deltaOverSet delta s qs = foldl (\acc q -> Set.union (delta q s) acc) Set.empty qs


-- |Return list of states reachable from given states with epsilon transitions alone
epsilonClosure :: (Ord q) => Delta q s -> Set.Set q -> Set.Set q -> (Set.Set q, Set.Set q)
epsilonClosure delta states visited
     | Set.null states = (states, visited)
     | otherwise = uncurry (epsilonClosure delta) $ swap $ unionAndApplyOverDiff delta' states visited
     where delta' = deltaOverSet delta Epsilon


epsilonClosure' :: (Ord q) => Delta q s -> Set.Set q -> Set.Set q
epsilonClosure' delta qs = snd $ epsilonClosure delta qs Set.empty

-- |If state is in return true if state is accepting else false
-- isAccepting :: Set.Set q -> q -> Bool

stateEpsilonClosure :: (Ord q) => Delta q s -> q -> Set.Set q
stateEpsilonClosure delta q = epsilonClosure' delta $ Set.singleton q


move :: (Ord q) => Delta q s -> q -> SigmaElem s -> Set.Set q
move delta q s = deltaOverSet delta s $ epsilonClosure' delta $ Set.singleton q


moveStates :: (Ord q) => Delta q s -> Set.Set q -> SigmaElem s -> Set.Set q
moveStates delta qs s = foldl (\acc s -> Set.union s acc) Set.empty states
    where states = map (\q -> move delta q s) $ Set.toList qs


-- |Validate elements of stream are in Sigma, return either with error message or stream
-- streamInSigma :: Sigma s -> [s] -> Either [String] [s]

eval :: (Ord q, Ord s) => Delta q s -> q -> Set.Set q -> [s] -> Bool
eval delta q0 qas ss = isAccepting $ foldl ffunction (Set.singleton q0) ss
    where isAccepting qs = qs `Set.isSubsetOf` qas
          ffunction qs s = moveStates delta qs (Symbol s)

--simple functions and  delegate error handling
--have wrapper functions to operate over automata
--dumb implementation, no partial matching; valid only whole stream is valid.
--public interface for building automata and state transition function
