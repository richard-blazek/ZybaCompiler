module Errors (Fallible, failure, assert) where

type Fallible a = Either String a

failure :: Integer -> String -> Fallible a
failure line msg = Left $ "Line " ++ show (line + 1) ++ ": " ++ msg

assert :: Bool -> Integer -> String -> Fallible ()
assert True _ _ = return ()
assert False line msg = failure line msg