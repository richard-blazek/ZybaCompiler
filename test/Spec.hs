import Test.HUnit

import Lexer
import Parser
import Data.Ratio

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

lexerTest = TestCase (assertEqual "Tokenization of sample text"
    (tokenize "1+2\n222\"abc+-/*!%74漢語if^\nelse1while\"*8+5.-4.404/hhh_hh")
    [LiteralInteger 10 1, Operator Plus, LiteralInteger 10 2, LiteralInteger 10 222, LiteralString "abc+-/*!%74漢語if\nelse1while",
    Operator Multiply, LiteralInteger 10 8, Operator Plus, LiteralDecimal 10 5 0, Operator Minus,
    LiteralDecimal 10 4404 3, Operator Divide, Identifier "hhh_hh"])

lexerTest2 = TestCase (assertEqual "Tokenization of mathematical expressions"
    (tokenize "16rFF--11r5 0\n7//5->var    **4^5&7>=4<=2r0.11~7<-<<fu:16r10")
    [LiteralInteger 16 255, Operator Minus, Operator Minus, LiteralInteger 11 5, LiteralInteger 10 0, LiteralInteger 10 7, Operator IntDivide,
    LiteralInteger 10 5, Operator Assign, Identifier "var", Operator RaiseToThePowerOf, LiteralInteger 10 4, Operator Xor,
    LiteralInteger 10 5, Operator And, LiteralInteger 10 7, Operator GreaterThanOrEqualTo, LiteralInteger 10 4, Operator LowerThanOrEqualTo,
    LiteralDecimal 2 3 2, Operator NotEqual, LiteralInteger 10 7, Operator LowerThan, Operator Minus, Operator LowerThan,
    Operator LowerThan, Identifier "fu", Operator Apply, LiteralInteger 16 16])

lexerTest3 = TestCase (assertEqual "Tokenization of comments"
    (tokenize "{This is a comment{which shou\"ld be ignored}and it is}\"I said: ^\"Ho, ho^\" and {it} worked!\"{}7")
    [LiteralString "I said: \"Ho, ho\" and {it} worked!", LiteralInteger 10 7])

parserTest = TestCase (assertEqual "Parsing first mathematical expression" (parse (tokenize "((13))")) (Integer 13))

parserTest2 = TestCase (assertEqual "Parsing second mathematical expression"
    (parse (tokenize "1/(1+1/(3.1416))"))
    (BinaryOperation Divide (Integer 1) (BinaryOperation Divide (BinaryOperation Plus (Integer 1) (Integer 1))
    (Rational (31416 % 10000)))))

parserTest3 = TestCase (assertEqual "Parsing second mathematical expression"
    (parse (tokenize "my_var1+(\"lit^eral\"{comment}/(3**4))"))
    (BinaryOperation Plus (Variable "my_var1") (BinaryOperation Divide (String "literal")
    (BinaryOperation RaiseToThePowerOf (Integer 3) (Integer 4)))))

parserTest4 = TestCase (assertEqual "Parsing \"real\" code"
    (parse (tokenize "\
\   fun fact (n)\
\       1 -> result\
\       [n] -> i\
\\
\       while [i] > 1\
\           [result] * [i] -> result\
\           [i] - 1 -> i\
\       end\
\       [result]\
\   end\
\   5 -> num\
\   if [num] < 10\
\       10\
\   elif [num] > 15\
\       15\
\   else\
\       [num]\
\   end -> num"))
    [
        Function "fact" ["n"] [
            BinaryOperation Assign (Integer 1) (Variable "result"),
            BinaryOperation Assign (Dereference (Variable "n")) (Variable "i"),
            WhileLoop (BinaryOperation GreaterThan (Dereference (Variable "i")) (Integer 1)) [
                BinaryOperation Assign (BinaryOperation Multiply (Dereference (Variable "result")) (Dereference (Variable "i"))) (Variable "result"),
                BinaryOperation Assign (BinaryOperation Minus (Dereference (Variable "i")) (Integer 1)) (Variable "i")
            ],
            Dereference (Variable "result")
        ],
        BinaryOperation Assign (Integer 5) (Variable "num"),
        BinaryOperation Assign (IfStatement [
            (BinaryOperation LowerThan (Dereference (Variable "num")) (Integer 10), [Integer 10]),
            (BinaryOperation GreaterThan (Dereference (Variable "num")) (Integer 15), [Integer 15])
        ] (Just [Dereference (Variable "num")])) (Variable "num")
    ])

tests = TestList [lexerTest, lexerTest2, lexerTest3, parserTest, parserTest2, parserTest3, parserTest4]