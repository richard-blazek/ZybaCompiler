module Main where

import System.IO
import System.Environment

import Lexer (tokenize)
import Parser (parse)
import Semantics (analyse)

main :: IO ()
main = do
    line <- getLine
    putStrLn (show $ analyse $ parse $ tokenize line)
