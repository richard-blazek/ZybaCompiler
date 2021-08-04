import Test.HUnit

import Lexer
import Parser
import Algorithms
import Data.Ratio

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

testEqual t x y = TestCase $ assertEqual t x y
lexerTest = testEqual "Tokenization of sample text"
    [LiteralInteger 1, Operator Plus, LiteralInteger 2, LiteralInteger 222, LiteralString "abc+-/*!%74漢語if^\nelse1while",
    Operator Multiply, LiteralInteger 8, Operator Plus, LiteralDecimal $ 5 % 1, Operator Minus,
    LiteralDecimal $ 4404 % 1000, Operator Divide, Identifier "abh_hc"]
    (tokenize "1+2\n222\"abc+-/*!%74漢語if^\nelse1while\"*8+5.-4.404/abh_hc")

lexerTest2 = testEqual "Tokenization of mathematical expressions"
    [Operator BracketOpen, LiteralInteger 255, Operator Minus, Operator Minus, LiteralInteger 5, LiteralInteger 0,
    LiteralInteger 7, Operator IntDivide, LiteralInteger 5, Operator Assign, Identifier "var", Operator RaiseToThePowerOf,
    LiteralInteger 4, Operator Xor, LiteralInteger 5, Operator And, LiteralInteger 7, Operator GreaterThanOrEqualTo,
    LiteralInteger 4, Operator LowerThanOrEqualTo, LiteralDecimal $ 3 % 4, Operator NotEqual, LiteralInteger 7, Operator LowerThan,
    Operator Minus, Operator ShiftLeft, Identifier "fu", Operator BracketOpen, LiteralInteger 16, Operator BracketClose, Operator IntDivide]
    (tokenize "[16rFF--11r5 0\n7:5->var    ^4'5&7>=4<=2r0.11~7< - <<fu[16r10];\n:")

lexerTest3 = testEqual "Tokenization of comments"
    [LiteralString "I said: \"Ho, ho\" and ;it\n worked!", LiteralInteger 7]
    (tokenize ";This is a comment which shou\"ld be ignored and it is\n\"I said: \"\"Ho, ho\"\" and ;it\n worked!\";\n7")

lexerTest4 = testEqual "Tokenization of real code"
    [Keyword Fun, Identifier "power", Operator BracketOpen, Identifier "n", Identifier "exp", Operator BracketClose, Keyword If,
    Identifier "@", Operator BracketOpen, Identifier "exp", Operator BracketClose, Operator Equal, LiteralInteger 0, LiteralInteger 1,
    Keyword Else, Identifier "@", Operator BracketOpen, Identifier "n", Operator BracketClose, Operator Multiply, Identifier "power",
    Operator BracketOpen, Identifier "@", Operator BracketOpen, Identifier "n", Operator BracketClose,
    Identifier "@", Operator BracketOpen, Identifier "exp", Operator BracketClose,
    Operator Minus, LiteralInteger 1, Operator BracketClose, Keyword End, Keyword End]
    (tokenize "fun power [n exp] if @[exp] = 0 1 else @[n] * power[@[n] @[exp] - 1] end end")

dereference x = FunctionCall (Variable "@") [x]

parserTest = testEqual "Parsing first mathematical expression" [Integer 13] (parse $ tokenize "((13))")

parserTest2 = testEqual "Parsing second mathematical expression"
    [BinaryOperation Divide (Integer 1) $ BinaryOperation Divide (BinaryOperation Plus (Integer 1) $ Integer 1) $ Rational $ 31416 % 10000]
    (parse $ tokenize "1/(1+1/(3.1416))")

parserTest3 = testEqual "Parsing second mathematical expression"
    [BinaryOperation Plus (Variable "my_var1") $ BinaryOperation Divide (String "literal")
    $ BinaryOperation RaiseToThePowerOf (Integer 3) $ Integer 4]
    (parse $ tokenize "my_var1+(\"literal\";comment\n/(3^4))")

parserTest4 = testEqual "Parsing \"real\" code"
    [
        Function "fact" ["n"] [
            BinaryOperation Assign (Integer 1) $ Variable "result",
            BinaryOperation Assign (dereference $ Variable "n") $ Variable "i",
            WhileExpression (BinaryOperation GreaterThan (dereference $ Variable "i") $ Integer 1) [
                BinaryOperation Assign (BinaryOperation Multiply (dereference $ Variable "result") $ dereference $ Variable "i") $ Variable "result",
                BinaryOperation Assign (BinaryOperation Minus (dereference $ Variable "i") $ Integer 1) $ Variable "i"
            ],
            dereference $ Variable "result"
        ],
        BinaryOperation Assign (Integer 5) $ Variable "num",
        BinaryOperation Assign (IfExpression [
            (BinaryOperation LowerThan (dereference $ Variable "num") $ Integer 10, [Integer 10]),
            (BinaryOperation GreaterThan (dereference $ Variable "num") $ Integer 15, [Integer 15])
        ] [dereference $ Variable "num"]) $ Variable "num"
    ]
    (parse $ tokenize "\
\   fun fact[n]\
\       1 -> result\
\       @[n] -> i\
\       while @[i] > 1\
\           @[result] * @[i] -> result\
\           @[i] - 1 -> i\
\       end\
\       @[result]\
\   end\
\   5 -> num\
\   if @[num] < 10\
\       10\
\   elif @[num] > 15\
\       15\
\   else\
\       @[num]\
\   end -> num")

parserTest5 = testEqual "Parsing another real code"
    [
        Function "power" ["n", "exp"] [
            IfExpression [
                (BinaryOperation Equal (dereference $ Variable "exp") $ Integer 0, [Integer 1])
            ] [
                BinaryOperation Multiply (dereference $ Variable "n") $ FunctionCall (Variable "power") [
                    dereference $ Variable "n",
                    BinaryOperation Minus (dereference $ Variable "exp") $ Integer 1
                ]
            ]
        ]
    ]
    (parse $ tokenize "fun power[n exp] if @[exp] = 0 1 else @[n] * power[@[n] @[exp] - 1] end end")

algorithmTest = testEqual "Testing distinctSort" (distinctSort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9]) [1, 2, 3, 4, 5, 6, 8, 9]

tests = TestList [lexerTest, lexerTest2, lexerTest3, lexerTest4, parserTest, parserTest2, parserTest3, parserTest4, parserTest5]