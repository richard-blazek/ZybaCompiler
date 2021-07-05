import Test.HUnit

import Lexer

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

tests = TestList [testLexer]

testLexer = TestCase (assertEqual "lexical analyser should tokenize this text correctly," 
        (tokenize "1+2\n222\"abc+-/*!%74漢語if\\else1while\"*8+5.-4.44/hhh_hh")
        [LiteralInteger 1, Operator PlusOperator, LiteralInteger 2, LiteralInteger 222, LiteralString "abc+-/*!%74漢語if\\else1while",
          Operator MultiplyOperator, LiteralInteger 8, Operator PlusOperator, LiteralFloat 5 0, Operator MinusOperator,
            LiteralFloat 444 2, Operator DivideOperator, Identifier "hhh", Identifier "hh"])
