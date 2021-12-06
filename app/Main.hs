module Main where

import qualified System.IO as IO
import qualified System.Environment as Env

import Lexer (tokenize)
import Parser (parse)
import Semantics (analyse)
import Codegen (generate)
import Functions ((??), (!?))

process :: Monad m => (a -> c) -> m a -> (c -> m b) -> m b
process transformation input output = fmap transformation input >>= output

compile :: String -> String
compile s = case parse (tokenize s) >>= analyse >>= generate of
  Left err -> "Compilation failed\n" ++ err
  Right code -> code

run :: [String] -> IO ()
run args = process compile input output
    where
        input = fmap IO.readFile (args !? 0) ?? IO.getLine
        output = fmap IO.writeFile (args !? 1) ?? IO.putStrLn

main :: IO ()
main = Env.getArgs >>= run
