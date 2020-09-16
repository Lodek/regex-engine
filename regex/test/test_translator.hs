import Test.HUnit
import Translator
import Automata
import qualified Data.Set as Set

singletonDeltaTest = TestLabel "singletonDelta test" test
    where test = TestCase $ assertEqual "" expected $ f q0 symbol
          symbol = Symbol 'a'
          q0 = 1
          qa = 2
          f = singletonDelta symbol q0 qa
          expected = Set.singleton qa


-- |Test whether the accepting state of d1 is equivalent to the initial state of d2
concatDeltaTest = TestLabel "concatDelta test" test 
    where test = TestCase $ assertEqual "" expected $ f state_input symbol_input
          q10 = 1
          q1a = 2
          q1s = Set.fromList [q10, q1a]
          d1 = singletonDelta (Symbol 'a') q10 q1a
          q20 = 3
          q2a = 4
          q2s = Set.fromList [q20, q2a]
          d2 = singletonDelta (Symbol 'b') q20 q2a
          f = concatDelta d1 d2 q1s q2s q1a q20
          state_input = 2
          symbol_input = Symbol 'b'
          expected = Set.singleton q2a




tests = TestList [singletonDeltaTest, concatDeltaTest]
