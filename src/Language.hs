module Language (Type (..), Primitive (..), getType, isPrimitive, getPrimitive, removePrimitives, getResultType) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Errors (Fallible, failure, assert)
import Functions (joinShow)

data Type = Int | Bool | Float | Text | Void | Fun [Type] Type deriving (Eq)
data Primitive = Add | Sub | Mul | Div | IntDiv | Rem | And | Or | Xor | Eq | Neq | Lt | Gt | Le | Ge
  | Pow | Not | ToInt | ToFloat | ToBool | ToText deriving (Eq, Ord)

instance Show Primitive where
  show primitive = primitivesReversed Map.! primitive
    where primitivesReversed = Map.fromList $ map (\(k, v) -> (v, k)) $ Map.toList primitives

instance Show Type where
  show Int = "Int"
  show Bool = "Bool"
  show Float = "Float"
  show Text = "Text"
  show Void = "Void"
  show (Fun args result) = "Fun[" ++ joinShow "," (args ++ [result]) ++ "]"

primitives :: Map.Map String Primitive
primitives = Map.fromList [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("//", IntDiv), ("%", Rem), ("&", And), ("|", Or), ("^", Xor), ("==", Eq),
  ("!=", Neq), ("<", Lt), (">", Gt), ("<=", Le), (">=", Ge), ("**", Pow), ("not", Not), ("Int", ToInt), ("Float", ToFloat), ("Bool", ToBool), ("Text", ToText)]

primitivesSet :: Set.Set String
primitivesSet = Map.keysSet primitives

getType :: Integer -> String -> [Type] -> Fallible Type
getType _ "Int" [] = Right Int
getType _ "Float" [] = Right Float
getType _ "Bool" [] = Right Bool
getType _ "Text" [] = Right Text
getType _ "Void" [] = Right Void
getType _ "Fun" types@(_ : _) = Right $ Fun (init types) (last types)
getType line name [] = failure line $ "Invalid type: " ++ name
getType line name types = failure line $ "Invalid type: " ++ name ++ "[" ++ joinShow "," types ++ "]"

isPrimitive :: String -> Bool
isPrimitive = flip Map.member primitives

getPrimitive :: Integer -> String -> Fallible Primitive
getPrimitive line name = case Map.lookup name primitives of
  Just primitive -> Right primitive
  Nothing -> failure line $ "Identifier " ++ name ++ " does not denote anyting in this context"

removePrimitives :: Set.Set String -> Set.Set String
removePrimitives = flip Set.difference primitivesSet

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
  (ToInt, [a]) | a `elem` [Int, Float, Bool, Text] -> Right Int
  (ToFloat, [a]) | a `elem` [Int, Float, Bool, Text] -> Right Float
  (ToBool, [a]) | a `elem` [Int, Float, Bool, Text] -> Right Bool
  (ToText, [a]) | a `elem` [Int, Float, Bool, Text] -> Right Text
  _ -> failure line $ "Primitive " ++ show primitive ++ " does not accept arguments of types " ++ joinShow ", " args
