module Translator where

import qualified Data.Set as Set
import Automata

cond :: [(Bool, a)] -> a
cond [] = error "cond list has no true"
cond ((bool, exp):xs) = if bool then exp else cond xs

extendAlphabet :: (Ord s) => Alphabet s -> Alphabet s -> Alphabet s
extendAlphabet = Set.union


singletonNFA :: (Ord s) => SigmaElem s -> Automata s
singletonNFA symbol = NFA alphabet states q0 qas delta
    where alphabet = Set.singleton symbol
          q0 = 1
          qa = 2
          states = Set.fromList [q0, qa]
          qas = Set.singleton qa
          delta = singletonDelta symbol q0 qa

singletonDelta :: SigmaElem s -> State -> State -> Delta s
singletonDelta s q0 qa = (\q s' -> case q of q0 -> case s' of s -> Set.singleton qa)

--concatNFA :: (Ord s) => Automata s -> Automata s -> Automata s

-- |Construct new delta for a concatenation operation. New Delta is constructed
-- |by mapping the accepting state of a into the start state of b.
-- |return new Delta
concatDelta :: Delta s -> Delta s -> Set.Set State -> Set.Set State -> State -> State -> Delta s
concatDelta a b qas qbs qaa qb0 = 
    (\q s -> case q of 
               qaa -> b qb0 s
               _   -> cond [(q `Set.member` qas, a q s),
                            (True, b q s)])
