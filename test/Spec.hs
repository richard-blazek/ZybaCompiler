import Test.HUnit

import qualified FunctionsSpec
import qualified LexerSpec
import qualified ParserSpec
import qualified SemanticsSpec

tests = TestList $ FunctionsSpec.tests ++ LexerSpec.tests ++ ParserSpec.tests ++ SemanticsSpec.tests

main :: IO ()
main = runTestTT tests >> return ()
