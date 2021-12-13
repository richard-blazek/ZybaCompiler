module Errors (Fallible, failure, failures) where

type Fallible a = Either String a

failure :: Integer -> String -> Fallible a
failure line msg = Left $ "Line " ++ show (line + 1) ++ ": " ++ msg

failures :: [String] -> Fallible a
failures = Left . unlines