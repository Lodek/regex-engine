import Test.HUnit
import Parser

testGenToken = let input = "abc"
                   expected = [SToken 'a', SToken 'b', SToken 'c']
    in TestCase (assertEqual "" expected genTokens input)

testGenToken1 = let input = "ab*"
                   expected = [SToken 'a', SToken 'b', QtToken Kleene]
    in TestCase (assertEqual "" expected genTokens input)

testGenToken2 = let input = "ab|c"
                   expected = [SToken 'a', SToken 'b', OpToken Alternation, SToken 'c']
    in TestCase (assertEqual "" expected genTokens input)

testNormalizeStream1 = let input = [SToken 'a', SToken 'b', OpToken Alternation, SToken 'c']
                          expected = [SToken 'a', OpToken Concat, SToken 'b', OpToken Alternation, SToken 'c']
    in TestCase (assertEqual "" expected normalizeStream input)

testNormalizeStream2 = let input = [SToken 'a', SToken 'b', OpToken Kleene]
                          expected = [SToken 'a', OpToken Concat, SToken 'b', OpToken Kleene]
    in TestCase (assertEqual "" expected normalizeStream input)

testNormalizeStream2 = let input = [GroupBegin, SToken 'a', SToken 'b', GroupEnd, OpToken Kleene]
                          expected = [GroupBegin, SToken 'a', OpToken Concat, SToken 'b', GroupEnd, OpToken Kleene]
    in TestCase (assertEqual "" expected normalizeStream input)

testNormalizeStream3 = let input = [SToken 'a', SToken 'b', SToken 'c']
                          expected = [SToken 'a', OpToken Concat, SToken 'b', OpToken Concat, SToken 'c']
    in TestCase (assertEqual "" expected normalizeStream input)
