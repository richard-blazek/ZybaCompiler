module LexerSpec (tests) where

import Test.HUnit
import Lexer

testEqual t x y = TestCase $ assertEqual t x y
test1 = testEqual "Tokenization of the first code"
  [(0, LiteralInteger 10 1), (0, Operator "+"), (0, LiteralInteger 10 2), (1, LiteralInteger 10 222),
  (1, LiteralString "a+%74漢語if^\nelse1e"), (2, Operator "*"), (2, LiteralInteger 9 8), (2, Operator "+"),
  (2, LiteralRational 9 5 0), (2, Operator "-"), (2, LiteralRational 10 4404 3), (2, Operator "/"), (2, Word "abh_hc")]
  $ tokenize "1+2\n222\"a+%74漢語if^\nelse1e\"*9r8+9r5.-4.404/abh_hc"

test2 = testEqual "Tokenization of the second code"
  [(0, Separator '['), (0, LiteralInteger 16 255), (0, Operator "--"), (0, LiteralInteger 9 5), (0, LiteralInteger 10 0),
  (1, LiteralInteger 10 7), (1, Operator ":"), (1, LiteralInteger 10 5), (1, Operator "->"), (1, Word "var"),
  (1, Operator "^"), (1, LiteralInteger 10 4), (1, Operator "~"), (1, LiteralInteger 10 5), (1, Operator "&"),
  (1, LiteralInteger 10 7), (1, Operator ">="), (1, LiteralInteger 10 4), (1, Operator "<="), (1, LiteralRational 2 3 2),
  (1, Operator "!="), (1, LiteralInteger 10 7), (1, Operator "<"), (1, Operator "-"), (1, Operator "<<"), (1, Word "fu"),
  (1, Separator '['), (1, LiteralInteger 16 16), (1, Separator ']'), (2, Operator ":")]
  $ tokenize "[16rFF--9r5 0\n7:5->var  ^4~5&7>=4<=2r0.11!=7< - <<fu[16r10];\n:"

test3 = testEqual "Tokenization of the third code"
  [(0, LiteralString "I said: \"Ho, ho\" and ;it\n worked!"), (3, LiteralInteger 10 7)]
  $ tokenize ";Just a comment which i\"s ignored\n\"I said: \"\"Ho, ho\"\" and ;it\n worked!\";\n7"

test4 = testEqual "Tokenization of the fourth code"
  [(0, Word "fun"), (2, Word "power"), (2, Separator '['), (2, Word "n"), (2, Word "exp"), (2, Separator ']'), (2, Word "if"),
  (2, Word "f"), (2, Separator '['), (2, Word "exp"), (2, Separator ']'), (2, Operator "="), (2, LiteralInteger 10 0),
  (2, LiteralInteger 10 1), (2, Word "else"), (2, Word "f"), (2, Separator '['), (2, Word "n"), (2, Separator ']'), (2, Operator "*"),
  (2, Word "power"), (2, Separator '['), (2, Word "f"), (2, Separator '['), (2, Word "n"), (2, Separator ']'), (2, Word "f"),
  (2, Separator '['), (2, Word "exp"), (2, Separator ']'), (2, Operator "-"), (2, LiteralInteger 10 1), (3, Separator ']'),
  (3, Word "is"), (3, Word "is")]
  $ tokenize "fun\n\npower [n exp] if f[exp] = 0 1 else f[n] * power[f[n] f[exp] - 1]\nis is"

test5 = testEqual "Tokenization of an incorrent code"
  [(0, Word "stri"), (7, LiteralInteger 10 2), (7, LiteralString "###\"```"), (7, LiteralInteger 10 987), (7, Operator "<=="),
  (7, Operator ">"), (8, LiteralRational 8 25 0)]
  $ tokenize "stri#\n\n\n\n\n\n\n2\"###\"\"```\"987<== >\n`.8r31."

tests = [test1, test2, test3, test4, test5]
