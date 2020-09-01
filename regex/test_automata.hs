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

-- 
unionAndApplyOverDiffTest = TestLabel "unionAndApplyOverDiff test" test
    where test = TestCase $ assertEqual "" c $ f f' a b
          f = unionAndApplyOverDiff 
          f'= (\a -> a)
          a = Set.fromList [1,2,3]
          b = Set.fromList [3,5]
          c = (Set.fromList [1,2,3,5], Set.fromList [1,2])

deltaOverSetTest = TestLabel "deltaOverSet test" test
    where test = TestCase $ assertEqual "" expected $ f delta s qs 
          f = deltaOverSet 
          expected = Set.fromList [4,5,3]
          delta = (\q _ -> Set.singleton q)
          s = Symbol 0
          qs = Set.fromList [4,5,3]


epsilonClosureTest = TestLabel "epsilonClosure test" test
    where test = TestCase $ assertEqual "" expected $ f delta states visited
          f = epsilonClosure 
          visited = Set.empty
          states = Set.fromList [3,5,7]
          delta = (\q s -> if q < 8 then Set.fromList [q, q+1] else Set.empty)
          expected = (Set.empty, Set.fromList [3,4,5,6,7,8])

tests = TestList [buildSigmaTests, unionAndApplyOverDiffTest, deltaOverSetTest, epsilonClosureTest]
