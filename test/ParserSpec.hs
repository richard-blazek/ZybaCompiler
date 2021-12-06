module ParserSpec (tests) where

import Test.HUnit
import Data.Ratio
import Data.Either.Utils;
import Lexer (tokenize)
import Parser

pt = fromRight . parse . tokenize
op = Operation . Name
a \+ b = op "+" [a, b]
a \- b = op "-" [a, b]
a \* b = op "*" [a, b]
a \/ b = op "/" [a, b]
a \= b = op "=" [a, b]

testEqual t x y = TestCase $ assertEqual t x y
test1 = testEqual "Parsing the first code" [Function "some" [] $ Integer 13] $ pt "some is fun[] ((13))"

test2 = testEqual "Parsing the second code" 
    [Function "some" [] $ Integer 1 \/ (Integer 1 \+ Integer 1 \/ Rational (31416 % 10000))]
    $ pt "some is fun[] 1/(1+1/(3.1416))"

test3 = testEqual "Parsing the third code" 
    [Function "some" [] $ Name "my_var1" \+ (String "literal" \/ op "^" [Integer 3, Integer 4])]
    $ pt "some is fun[] my_var1+(\"literal\";comment\n/(3^4))"

test4 = testEqual "Parsing the fourth code" [
    Function "fact" ["n"] $ Condition (n \= zero) one $ n \* op "fact" [n \- one],
    Function "power" ["n", "e"] $ Condition (e \= zero) one $ n \* op "power" [n, e \- one],
    Function "getBit" ["n", "e"] $ op "&" [op ">>" [n, e], one],
    Function "signum" ["n"] $ Condition (n \= zero) zero $ Condition (op "<" [n, zero]) (zero \- one) one,
    Function "signOf" ["n"] $ Condition (n \= zero) zero $ Condition (op "<" [n, zero]) (zero \- one) one,
    Function "heron" ["a", "b", "c"] $ Assignment "s" (a \+ b \+ c \/ two) $ s \* (s \- a) \* (s \- b) \* (s \- c)]
    $ pt "\
\   fact is fun[n] if n = 0 1 n * fact[n-1]\
\   power is fun[n e] if e = 0 1 n * power[n e-1]\
\   getBit is fun[n e] (n >> e) & 1\
\   signum is fun[n] if (n = 0) 0 (if (n < 0) (0 - 1) 1)\
\   signOf is fun[n] if n = 0 0 if n < 0 0 - 1 1\
\   heron is fun[a b c]\
\       for s = a + b + c / 2\
\       s * (s - a) * (s - b) * (s - c)"
    where
        (zero, one, two) = (Integer 0, Integer 1, Integer 2)
        (n, e, s, a, b, c) = (Name "n", Name "e", Name "s", Name "a", Name "b", Name "c")

test5 = testEqual "Parsing the second code" [
    InvalidDeclaration "Expected Word \"fun\" but got Word \"not\"",
    Function "fun" ["a", "b"] $ InvalidExpression "Expected a value but reached the EOF"]
    $ pt "some is not fun is fun[a b]"

tests = [test1, test2, test3, test4]
