module CodegenSpec (tests) where

import Test.HUnit
import Lexer (tokenize)
import Parser (parse)
import Semantics (analyse)
import Codegen

compile :: String -> Fallible String
compile s = parse (tokenize s) >>= analyse >>= generate

testEqual t x y = TestCase $ assertEqual t x y

tests = []