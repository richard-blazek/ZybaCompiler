module Errors (Fallible, failure) where

type Fallible a = Either String a
failure :: Integer -> String -> Fallible a
failure line msg = Left $ "Line " ++ show line ++ ": " ++ msg
