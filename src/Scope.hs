module Scope (Scope, Type (..), primitives, empty, addConstant, addVariable, getType, getResultType) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Functions (joinShow)
import Errors (Fallible, failure, assert)

data Type = Int | Bool | Float | String | Void | Projection [Type] Type deriving (Eq, Read, Show)
data Scope = Scope (Map.Map String (Type, Bool))

primitives :: Set.Set String
primitives = Set.fromList ["+", "-", "*", "/", "%", "&", "|", "^", "=", "!=", "<", ">", "<=", ">=", "**", "not"]

empty :: Scope
empty = Scope Map.empty

getType :: Integer -> String -> Scope -> Fallible Type
getType line name (Scope scope) = case Map.lookup name scope of
  Just (type', _) -> Right type'
  Nothing | Set.member name primitives -> failure line $ "Identifier " ++ name ++ " denotes a primitive"
  Nothing -> failure line $ "Identifier " ++ name ++ " does not denote anything at all"

addConstant :: Integer -> String -> Type -> Scope -> Fallible Scope
addConstant line name type' (Scope scope)
  | Set.member name primitives || Map.member name scope = failure line $ "Redefinition of " ++ name
  | otherwise = Right $ Scope $ Map.insert name (type', False) scope

addVariable :: Integer -> String -> Type -> Scope -> Fallible (Scope, Bool)
addVariable line name type' (Scope scope) = case Map.lookup name scope of
  Nothing -> Right (Scope $ Map.insert name (type', True) scope, True)
  Just (_, False) -> failure line $ "Assigning to " ++ show name ++ " which is a constant"
  Just (previousType, True) | type' == previousType -> Right (Scope scope, False)
  Just (previousType, True) -> failure line $ "Assigning to " ++ name ++ " which is of type " ++ show previousType ++ ", not " ++ show type'

getResultType :: Integer -> String -> [Type] -> Scope -> Fallible Type
getResultType line name args (Scope scope)
  | Set.member name primitives = case (name, args) of
    ("+", [Int, Int]) -> Right Int
    ("+", [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
    ("-", [Int, Int]) -> Right Int
    ("-", [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
    ("*", [Int, Int]) -> Right Int
    ("*", [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
    ("/", [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
    ("//", [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Int
    ("%", [Int, Int]) -> Right Int
    ("%", [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
    ("&", [Int, Int]) -> Right Int
    ("&", [Bool, Bool]) -> Right Bool
    ("|", [Int, Int]) -> Right Int
    ("|", [Bool, Bool]) -> Right Bool
    ("^", [Int, Int]) -> Right Int
    ("^", [Bool, Bool]) -> Right Bool
    ("=", [Int, Int]) -> Right Bool
    ("=", [Bool, Bool]) -> Right Bool
    ("=", [a, b]) | all (`elem` [Int, Float]) [a, b] -> failure line "Do not compare floats for equality. Learn more: https://stackoverflow.com/questions/1088216/whats-wrong-with-using-to-compare-floats-in-java"
    ("!=", [Int, Int]) -> Right Bool
    ("!=", [Bool, Bool]) -> Right Bool
    ("!=", [a, b]) | all (`elem` [Int, Float]) [a, b] -> failure line "Do not compare floats for equality. Learn more: https://stackoverflow.com/questions/1088216/whats-wrong-with-using-to-compare-floats-in-java"
    ("<", [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Bool
    (">", [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Bool
    ("<=", [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Bool
    (">=", [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Bool
    ("**", [Int, Int]) -> Right Int
    ("**", [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
    ("not", [Int]) -> Right Int
    ("not", [Bool]) -> Right Bool
    ("Int", [a]) | a `elem` [Int, Float, Bool, String] -> Right Int
    ("Float", [a]) | a `elem` [Int, Float, Bool, String] -> Right Float
    ("Bool", [a]) | a `elem` [Int, Float, Bool, String] -> Right Bool
    ("String", [a]) | a `elem` [Int, Float, Bool, String] -> Right String
    _ -> failure line $ "Primitive " ++ name ++ " does not accept arguments of types " ++ joinShow ", " args
  | otherwise = case Map.lookup name scope of
    Just (Projection args' result, _) -> if args' == args then Right result else failure line $ "Function " ++ name ++ " does not accept these arguments"
    Just _ -> failure line $ "Identifier " ++ name ++ " does not denote a function"
    Nothing -> failure line $ "Identifier " ++ name ++ " does not denote anything"
