module LexerSpec (tests) where

import Test.HUnit
import Lexer

testEqual t x y = TestCase $ assertEqual t x y
test1 = testEqual "Tokenization of the first code"
    [LiteralInteger 10 1, Operator "+", LiteralInteger 10 2, LiteralInteger 10 222, LiteralString "a+%74漢語if^\nelse1e",
    Operator "*", LiteralInteger 9 8, Operator "+", LiteralRational 9 5 0, Operator "-", LiteralRational 10 4404 3,
    Operator "/", Word "abh_hc"]
    $ tokenize "1+2\n222\"a+%74漢語if^\nelse1e\"*9r8+9r5.-4.404/abh_hc"

test2 = testEqual "Tokenization of the second code"
    [Separator '[', LiteralInteger 16 255, Operator "--", LiteralInteger 9 5, LiteralInteger 10 0, LiteralInteger 10 7,
    Operator ":", LiteralInteger 10 5, Operator "->", Word "var", Operator "^", LiteralInteger 10 4, Operator "~",
    LiteralInteger 10 5, Operator "&", LiteralInteger 10 7, Operator ">=", LiteralInteger 10 4, Operator "<=",
    LiteralRational 2 3 2, Operator "!=", LiteralInteger 10 7, Operator "<", Operator "-", Operator "<<", Word "fu",
    Separator '[', LiteralInteger 16 16, Separator ']', Operator ":"]
    $ tokenize "[16rFF--9r5 0\n7:5->var    ^4~5&7>=4<=2r0.11!=7< - <<fu[16r10];\n:"

test3 = testEqual "Tokenization of the third code"
    [LiteralString "I said: \"Ho, ho\" and ;it\n worked!", LiteralInteger 10 7]
    $ tokenize ";Just a comment which i\"s ignored\n\"I said: \"\"Ho, ho\"\" and ;it\n worked!\";\n7"

test4 = testEqual "Tokenization of the fourth code"
    [Word "fun", Word "power", Separator '[', Word "n", Word "exp", Separator ']', Word "if", Word "f", Separator '[',
    Word "exp", Separator ']', Operator "=", LiteralInteger 10 0, LiteralInteger 10 1, Word "else", Word "f",
    Separator '[', Word "n", Separator ']', Operator "*", Word "power", Separator '[', Word "f", Separator '[',
    Word "n", Separator ']', Word "f", Separator '[', Word "exp", Separator ']', Operator "-", LiteralInteger 10 1,
    Separator ']', Word "is", Word "is"]
    $ tokenize "fun power [n exp] if f[exp] = 0 1 else f[n] * power[f[n] f[exp] - 1] is is"

test5 = testEqual "Tokenization of an incorrent code"
    [Word "stri", LiteralInteger 10 2, LiteralString "###\"```", LiteralInteger 10 987, Operator "<==", Operator ">",
    LiteralRational 8 25 0]
    $ tokenize "stri#2\"###\"\"```\"987<== >`.8r31."

tests = [test1, test2, test3, test4, test5]
