module SemanticsSpec (tests) where

import Test.HUnit

testEqual t x y = TestCase $ assertEqual t x y
tests = []