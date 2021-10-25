import Test.HUnit

import qualified FunctionsSpec
import qualified LexerSpec
import qualified ParserSpec
import qualified SemanticsSpec
import qualified CodegenSpec

tests = TestList $ concat [FunctionsSpec.tests, LexerSpec.tests, ParserSpec.tests, SemanticsSpec.tests, CodegenSpec.tests]

main :: IO ()
main = fmap (const ()) $ runTestTT tests
