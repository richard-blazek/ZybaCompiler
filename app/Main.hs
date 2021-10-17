module Main where

import System.IO
import System.Environment

import Lexer
import Parser

main :: IO ()
main = do
    line <- getLine
    putStrLn (show $ parse $ tokenize line)

