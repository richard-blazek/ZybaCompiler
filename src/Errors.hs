module Errors where

class Fallible t where
    invalid :: String -> t