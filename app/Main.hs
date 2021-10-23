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
        input = (args !? 0 >>= return . IO.readFile) ?? IO.getLine
        output = (args !? 1 >>= return . IO.writeFile) ?? IO.putStrLn

main :: IO ()
main = Env.getArgs >>= compile
