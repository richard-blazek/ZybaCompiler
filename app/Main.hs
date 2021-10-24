module Main where

import qualified System.IO as IO
import qualified System.Environment as Env

import Lexer (tokenize)
import Parser (parse)
import Semantics (analyse)
import Functions ((??), (!?))

transcribe :: (String -> String) -> IO String -> (String -> IO ()) -> IO ()
transcribe fn input output = input >>= output . fn

compile :: [String] -> IO ()
compile args = transcribe (show . analyse . parse . tokenize) input output
    where
        input = fmap IO.readFile (args !? 0) ?? IO.getLine
        output = fmap IO.writeFile (args !? 1) ?? IO.putStrLn

main :: IO ()
main = Env.getArgs >>= compile
