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

tests = TestList [genTokenTests, normalizeStreamTests]

