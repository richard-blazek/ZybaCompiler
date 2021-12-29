module Language (Type (..), Primitive (..), isPrimitive, getPrimitive, removePrimitives, getResultType, constants) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Errors (Fallible, failure, assert)
import Functions (join, split, (??))

data Type = Void | Int | Bool | Float | Text | Function [Type] Type | MapArray Type Type | IntArray Type | Record (Map.Map String Type) deriving (Eq, Show)
data Primitive = Add | Sub | Mul | Div | IntDiv | Rem | And | Or | Xor | Eq | Neq | Lt | Gt | Le | Ge | Pow | Not
  | AsInt | AsFloat | AsBool | AsText | Fun | Map | Array deriving (Eq, Ord)

instance Show Primitive where
  show primitive = primitivesReversed Map.! primitive
    where primitivesReversed = Map.fromList $ map (\(k, v) -> (v, k)) $ Map.toList primitives

primitives :: Map.Map String Primitive
primitives = Map.fromList [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("//", IntDiv), ("%", Rem), ("&", And), ("|", Or), ("^", Xor), ("==", Eq), ("!=", Neq),
  ("<", Lt), (">", Gt), ("<=", Le), (">=", Ge), ("**", Pow), ("not", Not), ("asInt", AsInt), ("asFloat", AsFloat), ("asBool", AsBool), ("asText", AsText),
  ("fun", Fun), ("map", Map), ("array", Array)]

primitivesSet :: Set.Set String
primitivesSet = Map.keysSet primitives

constants :: Map.Map String Type
constants = Map.fromList [("int", Int), ("bool", Bool), ("float", Float), ("text", Text), ("void", Void)]

isPrimitive :: String -> Bool
isPrimitive = flip Map.member primitives

removePrimitives :: Set.Set String -> Set.Set String
removePrimitives = flip Set.difference primitivesSet

getPrimitive :: Integer -> String -> Fallible Primitive
getPrimitive line name = fmap Right (Map.lookup name primitives) ?? failure line ("Identifier " ++ name ++ " does not denote anyting in this context")

getResultType :: Integer -> Primitive -> [Type] -> Fallible Type
getResultType line primitive args = case (primitive, args) of
  (Add, [Int, Int]) -> Right Int
  (Add, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
  (Add, [Text, Text]) -> Right Text
  (Sub, [Int, Int]) -> Right Int
  (Sub, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
  (Mul, [Int, Int]) -> Right Int
  (Mul, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
  (Div, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
  (IntDiv, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Int
  (Rem, [Int, Int]) -> Right Int
  (Rem, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
  (And, [Int, Int]) -> Right Int
  (And, [Bool, Bool]) -> Right Bool
  (Or, [Int, Int]) -> Right Int
  (Or, [Bool, Bool]) -> Right Bool
  (Xor, [Int, Int]) -> Right Int
  (Xor, [Bool, Bool]) -> Right Bool
  (Eq, [Int, Int]) -> Right Bool
  (Eq, [Bool, Bool]) -> Right Bool
  (Eq, [Text, Text]) -> Right Bool
  (Eq, [MapArray k1 v1, MapArray k2 v2]) -> do
    getResultType line Eq [k1, k2]
    getResultType line Eq [v1, v2]
    Right Bool
  (Eq, [a, b]) | any (== Float) [a, b] -> failure line "Do not compare floats for equality. Learn more: https://stackoverflow.com/questions/1088216/whats-wrong-with-using-to-compare-floats-in-java"
  (Neq, [Int, Int]) -> Right Bool
  (Neq, [Bool, Bool]) -> Right Bool
  (Neq, [Text, Text]) -> Right Bool
  (Neq, [a, b]) | any (== Float) [a, b] -> failure line "Do not compare floats for equality. Learn more: https://stackoverflow.com/questions/1088216/whats-wrong-with-using-to-compare-floats-in-java"
  (Lt, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Bool
  (Lt, [Text, Text]) -> Right Bool
  (Gt, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Bool
  (Gt, [Text, Text]) -> Right Bool
  (Le, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Bool
  (Le, [Text, Text]) -> Right Bool
  (Ge, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Bool
  (Ge, [Text, Text]) -> Right Bool
  (Pow, [Int, Int]) -> Right Int
  (Pow, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
  (Not, [Int]) -> Right Int
  (Not, [Bool]) -> Right Bool
  (AsInt, [a]) | a `elem` [Int, Float, Bool, Text] -> Right Int
  (AsFloat, [a]) | a `elem` [Int, Float, Bool, Text] -> Right Float
  (AsBool, [a]) | a `elem` [Int, Float, Bool, Text] -> Right Bool
  (AsText, [a]) | a `elem` [Int, Float, Bool, Text] -> Right Text
  (Fun, returned : args) -> Right $ Function args returned
  (Map, key : value : args) -> do
    assert (key `elem` [Int, Text]) line "Map keys must be either int or text"
    let pairs = split args
    assert (pairs /= Nothing) line "Missing value for the last key"
    assert (fmap (all (== (key, value))) pairs == Just True) line "All keys and values must have the specified type"
    Right $ MapArray key value
  (Array, value : args) -> do
    assert (all (== value) args) line "All values must have the specified type"
    Right $ IntArray value
  _ -> failure line $ "Primitive " ++ show primitive ++ " does not accept arguments of types " ++ join ", " args
