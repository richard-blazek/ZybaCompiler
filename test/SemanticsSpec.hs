module SemanticsSpec (tests) where

import Data.Map.Strict (fromList)
import Test.HUnit
import Lexer (tokenize)
import Parser (parse)
import Semantics

process = analyse . parse . tokenize
testEqual t x y = TestCase $ assertEqual t (fromList x) y

naryProjection n = Projection (iterate (Pair Int) Void !! n) Int
a \- b = Operation Int (Primitive (naryProjection 2) Subtract) [a, b]
a \+ b = Operation Int (Primitive (naryProjection 2) Add) [a, b]
a \* b = Operation Int (Primitive (naryProjection 2) Multiply) [a, b]
a \/ b = Operation Int (Primitive (naryProjection 2) Divide) [a, b]
a \= b = Operation Int (Primitive (naryProjection 2) Equal) [a, b]
call name args = Operation Int (Global (naryProjection $ length args) name) args
ifte a b c = Operation Int (Primitive (naryProjection 3) If) [a, b, c]
arg = Argument Int
int = Literal Int

test1 = testEqual "Analysing the first code" [
  ("heron", Function (naryProjection 3) ["a", "b", "c"] $ s \* (s \- arg "a") \* (s \- arg "b") \* (s \- arg "c")),
  ("fact", Function (naryProjection 1) ["n"] $
    ifte (arg "n" \= int 0) (int 1) (call "fact" [arg "n" \- int 1] \* arg "n"))]
  $ process "heron is fun[a b c] for s = a + b + c / 2 s * (s - a) * (s - b) * (s - c)\
\   fact is fun[n] if n = 0 1 fact[n - 1] * n"
  where s = arg "a" \+ arg "b" \+ arg "c" \/ int 2

test2 = testEqual "Analysing the second code" [
  ("pow", Function (naryProjection 2) ["n", "e"] $ call "power" [arg "n", arg "e", int 1]),
  ("power", Function (naryProjection 3) ["n", "exp", "acc"] $
    ifte (arg "exp" \= int 0) (arg "acc") $
    ifte (arg "exp" \\ int 2 \= int 1) (call "power" [arg "n", arg "exp" \- int 1, arg "acc" \* arg "n"]) $
    call "power" [arg "n", arg "exp" \/ int 2, arg "acc" \* arg "acc"])]
  $ process "pow is fun[n e] power[n e 1]\
\   power is fun[n exp acc]\
\   for p = power\
\   if exp = 0 acc\
\   if exp \\ 2 = 1 p[n exp - 1 acc * n]\
\   p[n exp / 2 acc * acc]"

tests = [test1, test2]