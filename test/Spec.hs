import Test.HUnit

import Lexer
import Parser
import Semantics
import Functions
import Data.Ratio

testEqual t x y = TestCase $ assertEqual t x y
lexerTest = testEqual "Tokenization of the first code"
    [LiteralInteger 10 1, Operator "+", LiteralInteger 10 2, LiteralInteger 10 222, LiteralString "a+%74漢語if^\nelse1e",
    Operator "*", LiteralInteger 9 8, Operator "+", LiteralDecimal 9 5 0, Operator "-", LiteralDecimal 10 4404 3,
    Operator "/", Word "abh_hc"]
    $ tokenize "1+2\n222\"a+%74漢語if^\nelse1e\"*9r8+9r5.-4.404/abh_hc"

lexerTest2 = testEqual "Tokenization of the second code"
    [Separator '[', LiteralInteger 16 255, Operator "--", LiteralInteger 9 5, LiteralInteger 10 0, LiteralInteger 10 7,
    Operator ":", LiteralInteger 10 5, Operator "->", Word "var", Operator "^", LiteralInteger 10 4, Operator "~",
    LiteralInteger 10 5, Operator "&", LiteralInteger 10 7, Operator ">=", LiteralInteger 10 4, Operator "<=",
    LiteralDecimal 2 3 2, Operator "!=", LiteralInteger 10 7, Operator "<", Operator "-", Operator "<<", Word "fu",
    Separator '[', LiteralInteger 16 16, Separator ']', Operator ":"]
    $ tokenize "[16rFF--9r5 0\n7:5->var    ^4~5&7>=4<=2r0.11!=7< - <<fu[16r10];\n:"

lexerTest3 = testEqual "Tokenization of the third code"
    [LiteralString "I said: \"Ho, ho\" and ;it\n worked!", LiteralInteger 10 7]
    $ tokenize ";Just a comment which i\"s ignored\n\"I said: \"\"Ho, ho\"\" and ;it\n worked!\";\n7"

lexerTest4 = testEqual "Tokenization of the fourth code"
    [Word "fun", Word "power", Separator '[', Word "n", Word "exp", Separator ']', Word "if", Word "f", Separator '[',
    Word "exp", Separator ']', Operator "=", LiteralInteger 10 0, LiteralInteger 10 1, Word "else", Word "f",
    Separator '[', Word "n", Separator ']', Operator "*", Word "power", Separator '[', Word "f", Separator '[',
    Word "n", Separator ']', Word "f", Separator '[', Word "exp", Separator ']', Operator "-", LiteralInteger 10 1,
    Separator ']', Word "is", Word "is"]
    $ tokenize "fun power [n exp] if f[exp] = 0 1 else f[n] * power[f[n] f[exp] - 1] is is"

lexerTest5 = testEqual "Tokenization of an incorrent code"
    [Word "stri", InvalidToken '#', LiteralInteger 10 2, LiteralString "###\"```", LiteralInteger 10 987, Operator "<==>",
    InvalidToken '`', InvalidToken '.', LiteralDecimal 8 25 0]
    $ tokenize "stri#2\"###\"\"```\"987<==>`.8r31."

parserTest = testEqual "Parsing the first code" (Program [Function "some" [] $ Integer 13])
    $ parse $ tokenize "some is fun[] ((13))"

parserTest2 = testEqual "Parsing the second code" (
    Program [
        Function "some" [] $
            Operation "/" (Integer 1) $ Operation "/" (Operation "+" (Integer 1) $ Integer 1)
            $ Rational $ 31416 % 10000])
    $ parse $ tokenize "some is fun[] 1/(1+1/(3.1416))"

parserTest3 = testEqual "Parsing the third code" (
    Program [
        Function "some" [] $
            Operation "+" (Name "my_var1") $ Operation "/" (String "literal")
            $ Operation "^" (Integer 3) $ Integer 4])
    $ parse $ tokenize "some is fun[] my_var1+(\"literal\";comment\n/(3^4))"

parserTest4 = testEqual "Parsing the fourth code" (
    let (zero, one, n, e, invoke) = (Integer 0, Integer 1, Name "n", Name "e", Call . Name) in
    Program [
        Function "fact" ["n"]
            $ Condition (Operation "=" n zero) one $ Operation "*" n $ invoke "fact" [Operation "-" n one],
        Function "power" ["n", "e"]
            $ Condition (Operation "=" e zero) one $ Operation "*" n $ invoke "power" [n, Operation "-" e one],
        Function "getBit" ["n", "e"] $ Operation "&" (Operation ">>" n e) one,
        Function "signum" ["n"]
            $ Condition (Operation "=" n zero) zero $ Condition (Operation "<" n zero) (Operation "-" zero one) one,
        Function "signOf" ["n"]
            $ Condition (Operation "=" n zero) zero $ Condition (Operation "<" n zero) (Operation "-" zero one) one,
        let (two, s, a, b, c) = (Integer 2, Name "s", Name "a", Name "b", Name "c") in
        Function "heron" ["a", "b", "c"]
            $ Assignment "s" (Operation "/" (Operation "+" (Operation "+" a b) c) two) $ (Operation "*" (Operation "*"
            (Operation "*" s $ Operation "-" s a) $ Operation "-" s b) $ Operation "-" s c)])
    $ parse $ tokenize "\
\   fact is fun[n] if n = 0 1 n * fact[n-1]\
\   power is fun[n e] if e = 0 1 n * power[n e-1]\
\   getBit is fun[n e] (n >> e) & 1\
\   signum is fun[n] if (n = 0) 0 (if (n < 0) (0 - 1) 1)\
\   signOf is fun[n] if n = 0 0 if n < 0 0 - 1 1\
\   heron is fun[a b c]\
\       for s = a + b + c / 2\
\       s * (s - a) * (s - b) * (s - c)"

parserTest5 = testEqual "Parsing the second code" (
    Program [
        InvalidDeclaration "Expected Word \"fun\" but got Word \"not\"",
        Function "fun" ["a", "b"] $ InvalidExpression "Expected a value but reached the EOF"])
    $ parse $ tokenize "some is not fun is fun[a b]"

algorithmTest = testEqual "Testing distinctSort" [1, 2, 3, 4, 5, 6, 7, 8, 9]
    $ distinctSort [7, 7, 7, 3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9]

algorithmTest2 = testEqual "Testing mergeSort" [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 7, 7, 7, 8, 9, 9]
    $ mergeSort [7, 7, 7, 3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9]

tests = TestList [
    lexerTest, lexerTest2, lexerTest3, lexerTest4, lexerTest5,
    parserTest, parserTest2, parserTest3, parserTest4,
    algorithmTest, algorithmTest2]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()