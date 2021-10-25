module CodegenSpec (tests) where

import Test.HUnit
import Lexer (tokenize)
import Parser (parse)
import Semantics (analyse)
import Codegen

compile :: String -> String
compile = generate . analyse . parse . tokenize

testEqual t x y = TestCase $ assertEqual t x y

tests = []