import Test.HUnit

import Lexer

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

tests = TestList [testLexer, testLexer2, testLexer3]

testLexer = TestCase (assertEqual "Lexical analyser should tokenize this text correctly"
        (tokenize "1+2\n222\"abc+-/*!%74漢語if^\nelse1while\"*8+5.-4.44/hhh_hh")
        [LiteralInteger 10 1, Operator Plus, LiteralInteger 10 2, LiteralInteger 10 222, LiteralString "abc+-/*!%74漢語if\nelse1while",
        Operator Multiply, LiteralInteger 10 8, Operator Plus, LiteralFloat 10 5 0, Operator Minus,
        LiteralFloat 10 444 2, Operator Divide, Identifier "hhh_hh"])

testLexer2 = TestCase (assertEqual "Mathematical expressions should be tokenized correctly"
        (tokenize "16rFF--11r5 0\n7//5->var    **4^5&7>=4<=2r0.11~7<-<<fu:16r10")
        [LiteralInteger 16 255, Operator Minus, Operator Minus, LiteralInteger 11 5, LiteralInteger 10 0, LiteralInteger 10 7, Operator IntDivide,
        LiteralInteger 10 5, Operator Assign, Identifier "var", Operator RaiseToThePowerOf, LiteralInteger 10 4, Operator Xor,
        LiteralInteger 10 5, Operator And, LiteralInteger 10 7, Operator GreaterThanOrEqualTo, LiteralInteger 10 4, Operator LowerThanOrEqualTo,
        LiteralFloat 2 3 2, Operator NotEqual, LiteralInteger 10 7, Operator LowerThan, Operator Minus, Operator LowerThan,
        Operator LowerThan, Identifier "fu", Operator Apply, LiteralInteger 16 16])

testLexer3 = TestCase (assertEqual "comments and strings should be tokenized correctly"
        (tokenize "{This is just a comment{which should be ignored}and I hope it will be}")
        [Empty])