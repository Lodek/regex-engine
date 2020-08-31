import Test.HUnit
import Automata
import qualified Data.Set as Set


buildSigmaFromListTest = TestLabel "buildFromSigma test with list" test
    where test = TestCase $ assertEqual "" b $ f a
          f = buildSigma 
          a = "abc"
          b = Set.fromList [Symbol 'a', Symbol 'b', Symbol 'c', Epsilon]


buildSigmaFromSetTest = TestLabel "buildFromSigma test" test
    where test = TestCase $ assertEqual "" b $ f a
          f = buildSigma 
          a = Set.fromList [1,2,3]
          b = Set.fromList [Symbol 1, Symbol 2, Symbol 3, Epsilon]


buildSigmaTests = TestList [buildSigmaFromListTest, buildSigmaFromSetTest]


tests = TestList [buildSigmaTests]
