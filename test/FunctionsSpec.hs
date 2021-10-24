module FunctionsSpec (tests) where

import Test.HUnit
import Functions

testEqual t x y = TestCase $ assertEqual t x y
test1 = testEqual "Testing distinctSort" [1, 2, 3, 4, 5, 6, 7, 8, 9]
    $ distinctSort [7, 7, 7, 3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9]

test2 = testEqual "Testing mergeSort" [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 7, 7, 7, 8, 9, 9]
    $ mergeSort [7, 7, 7, 3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9]

tests = [test1, test2]