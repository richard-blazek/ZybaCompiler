module Main where

import System.IO
import System.Environment

import Lexer

main :: IO ()
main = do
    line <- getLine
    putStrLn (unlines (map show (tokenize line)))

