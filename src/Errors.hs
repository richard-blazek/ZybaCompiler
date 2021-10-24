module Errors where

class Fallible t where
    invalid :: String -> t

data Error = Error Integer Integer String