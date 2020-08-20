import Test.HUnit
import Parser

testCaseFactory :: (Eq a, Eq b, Show b) => String -> (a, b) -> (a -> b) -> Test
testCaseFactory label (a, b) f = TestLabel label $ TestCase $ assertEqual "" b $ f a 

genTokenData = [("abc", [SToken 'a', SToken 'b', SToken 'c']),
                ("ab*", [SToken 'a', SToken 'b', QtToken Kleene]),
                ("ab|c", [SToken 'a', SToken 'b', OpToken Alternation, SToken 'c'])]

genTokenTests = TestList $ map (\d -> testCaseFactory "genTokenTest" d genTokens) genTokenData

normalizeStreamData = [([SToken 'a', SToken 'b', OpToken Alternation, SToken 'c'],
                            [SToken 'a', OpToken Concat, SToken 'b', OpToken Alternation, SToken 'c']),
                       ([SToken 'a', SToken 'b', QtToken Kleene],
                            [SToken 'a', OpToken Concat, SToken 'b', QtToken Kleene]),
                       ([GroupBegin, SToken 'a', SToken 'b', GroupEnd, QtToken Kleene],
                            [GroupBegin, SToken 'a', OpToken Concat, SToken 'b', GroupEnd, QtToken Kleene]),
                       ([SToken 'a', SToken 'b', SToken 'c'],
                            [SToken 'a', OpToken Concat, SToken 'b', OpToken Concat, SToken 'c'])]
normalizeStreamTests = TestList $ map (\d -> testCaseFactory "normalizeStreamTest" d normalizeStream) normalizeStreamData


evenGroupPredicateData = [([GroupBegin, SToken 'a', GroupEnd], True),
                          ([GroupBegin, SToken 'b', GroupEnd, GroupEnd], False),
                          ([GroupBegin, GroupBegin, SToken 'b', GroupEnd, GroupEnd], True)]
evenGroupPredicateTest = TestList $ map (\d -> testCaseFactory "evenGroupPredicateTest" d evenGroupPredicate) evenGroupPredicateData


uniqueQuantifierPredicateData = [([SToken 'a', QtToken Kleene], True),
                                 ([SToken 'a', QtToken Kleene, QtToken Kleene], False)]

uniqueQuantifierPredicateTest = TestList $ map (\d -> testCaseFactory "uniqueQuantifierPredicateTest" d uniqueQuantifierPredicate) uniqueQuantifierPredicateData

sortAndTreefyData = [([SToken 'a', OpToken Concat, SToken 'b'],
                          [Right $ Leaf 'a', Left Concat, Right $ Leaf 'b']),
                     ([SToken 'a', QtToken Kleene, OpToken Concat, SToken 'b'], 
                          [Right $ QuantifierLeaf (Leaf 'a') Kleene, Left Concat, Right $ Leaf 'b'])]
sortAndTreefyTest = TestList $ map (\d -> testCaseFactory "sortAndTreefyTest" d sortAndTreefy) sortAndTreefyData

tests = TestList [genTokenTests, normalizeStreamTests, evenGroupPredicateTest, uniqueQuantifierPredicateTest, sortAndTreefyTest]

