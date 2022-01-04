module Main where

import qualified System.IO as IO
import qualified System.Environment as Env

import Lexer (tokenize)
import Parser (parse)
import Semantics (analyse)
import Codegen (generate)
import Functions ((??), (!?))
import Fallible (Fallible (..))

process :: Monad m => (a -> c) -> m a -> (c -> m b) -> m b
process transformation input output = fmap transformation input >>= output

compile :: String -> String
compile s = case fmap generate $ parse (tokenize s) >>= analyse of
  Error err -> "Compilation failed\n" ++ err
  Ok code -> code

run :: [String] -> IO ()
run args = process compile input output
  where input = fmap IO.readFile (args !? 0) ?? IO.getLine
        output = fmap IO.writeFile (args !? 1) ?? IO.putStrLn

main :: IO ()
main = Env.getArgs >>= run
