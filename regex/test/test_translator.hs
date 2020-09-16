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



tests = TestList [singletonDeltaTest]
