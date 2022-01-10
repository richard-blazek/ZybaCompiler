module Language (Type (..), Primitive (..), primitiveCall, fieldAccess, removePrimitives, constants) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Fallible (Fallible (..), failure, assert)
import Functions (join, split, (??=), zipMaps)

data Type = Void | Int | Bool | Float | Text | Function [Type] Type | Dictionary Type Type | Vector Type | Record (Map.Map String Type) deriving (Eq, Show)
data Primitive = Add | Sub | Mul | Div | IntDiv | Rem | And | Or | Xor | Eq | Neq | Lt | Gt | Le | Ge | Pow | Not
  | AsInt | AsFloat | AsBool | AsText | Fun | Dict | List | Set | Get | Has | Size | Concat | Append | Sized | Sort
  | Join deriving (Eq, Ord)

instance Show Primitive where
  show primitive = primitivesReversed Map.! primitive
    where primitivesReversed = Map.fromList $ map (\(k, v) -> (v, k)) $ Map.toList primitives

primitives :: Map.Map String Primitive
primitives = Map.fromList [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("//", IntDiv), ("%", Rem), ("&", And), ("|", Or), ("^", Xor), ("==", Eq), ("!=", Neq),
  ("<", Lt), (">", Gt), ("<=", Le), (">=", Ge), ("**", Pow), ("not", Not), ("asInt", AsInt), ("asFloat", AsFloat), ("asBool", AsBool), ("asText", AsText),
  ("fun", Fun), ("dict", Dict), ("list", List), ("set", Set), ("get", Get), ("has", Has), ("size", Size), ("concat", Concat), ("append", Append), ("sized", Sized),
  ("sort", Sort), ("join", Join)]

primitivesSet :: Set.Set String
primitivesSet = Map.keysSet primitives

constants :: Map.Map String Type
constants = Map.fromList [("int", Int), ("bool", Bool), ("float", Float), ("text", Text), ("void", Void)]

removePrimitives :: Set.Set String -> Set.Set String
removePrimitives = (`Set.difference` primitivesSet)

getResultType :: Integer -> Primitive -> [Type] -> Fallible Type
getResultType line primitive args = case (primitive, args) of
  (Add, [Int, Int]) -> Ok Int
  (Add, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Ok Float
  (Add, [Text, Text]) -> Ok Text
  (Add, [Vector v1, Vector v2]) | v1 == v2 -> Ok $ Vector v1
  (Sub, [Int, Int]) -> Ok Int
  (Sub, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Ok Float
  (Mul, [Int, Int]) -> Ok Int
  (Mul, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Ok Float
  (Div, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Ok Float
  (IntDiv, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Ok Int
  (Rem, [Int, Int]) -> Ok Int
  (Rem, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Ok Float
  (And, [Int, Int]) -> Ok Int
  (And, [Bool, Bool]) -> Ok Bool
  (Or, [Int, Int]) -> Ok Int
  (Or, [Bool, Bool]) -> Ok Bool
  (Or, [Dictionary k1 v1, Dictionary k2 v2]) | v1 == v2 && k1 == k2 -> Ok $ Dictionary k1 v1
  (Or, [Record f1, Record f2]) -> Ok $ Record $ Map.union f2 f1
  (Xor, [Int, Int]) -> Ok Int
  (Xor, [Bool, Bool]) -> Ok Bool
  (Eq, [Int, Int]) -> Ok Bool
  (Eq, [Bool, Bool]) -> Ok Bool
  (Eq, [Text, Text]) -> Ok Bool
  (Eq, [Vector v1, Vector v2]) -> getResultType line Eq [v1, v2]
  (Eq, [Dictionary k1 v1, Dictionary k2 v2]) -> getResultType line Eq [k1, k2] >> getResultType line Eq [v1, v2]
  (Eq, [Record f1, Record f2]) -> do
    mapM (getResultType line Eq) $ map (\(a, b) -> [a, b]) $ Map.elems $ zipMaps f1 f2
    Ok Bool
  (Eq, [a, b]) | any (== Float) [a, b] -> failure line "Do not compare floats for equality. Learn more: https://stackoverflow.com/questions/1088216/whats-wrong-with-using-to-compare-floats-in-java"
  (Neq, compared) -> getResultType line Eq compared
  (Lt, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Ok Bool
  (Lt, [Text, Text]) -> Ok Bool
  (Gt, compared) -> getResultType line Lt compared
  (Le, compared) -> getResultType line Lt compared
  (Ge, compared) -> getResultType line Lt compared
  (Pow, [Int, Int]) -> Ok Int
  (Pow, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Ok Float
  (Not, [Int]) -> Ok Int
  (Not, [Bool]) -> Ok Bool
  (AsInt, [a]) | a `elem` [Int, Float, Bool, Text] -> Ok Int
  (AsFloat, [a]) | a `elem` [Int, Float, Bool, Text] -> Ok Float
  (AsBool, [a]) | a `elem` [Int, Float, Bool, Text] -> Ok Bool
  (AsText, [a]) | a `elem` [Int, Float, Bool, Text] -> Ok Text
  (AsText, [Vector v]) -> getResultType line AsText [v]
  (AsText, [Dictionary k v]) -> getResultType line AsText [v]
  (AsText, [Record fields]) -> do
    mapM (getResultType line AsText . (:[])) $ Map.elems fields
    Ok Text
  (Fun, returned : args) -> Ok $ Function args returned
  (List, value : args) -> do
    assert (all (== value) args) line "All values must have the specified type"
    Ok $ Vector value
  (Dict, key : value : args) -> do
    assert (key `elem` [Int, Text]) line "Map keys must be either int or text"
    let pairs = split args
    assert (pairs /= Nothing) line "Missing value for the last key"
    assert (fmap (all (== (key, value))) pairs == Just True) line "All keys and values must have the specified type"
    Ok $ Dictionary key value
  (Get, [Vector value, Int]) -> Ok value
  (Get, [Dictionary key value, index]) | index == key -> Ok value
  (Set, [Vector value, Int, assigned]) | assigned == value -> Ok Void
  (Set, [Dictionary key value, index, assigned]) | index == key && assigned == value -> Ok Void
  (Has, [Dictionary key value, index]) | index == key -> Ok Bool
  (Size, [Text]) -> Ok Int
  (Size, [Vector _]) -> Ok Int
  (Size, [Dictionary _ _]) -> Ok Int
  (Concat, Vector v : args) | all (`elem` [Vector v, v]) args -> Ok $ Vector v
  (Append, Vector v : args) | all (`elem` [Vector v, v]) args -> Ok Void
  (Sized, [Vector v, Int]) -> Ok $ Vector v
  (Sized, [Vector v, Int, v2]) | v2 == v -> Ok $ Vector v
  (Sort, Vector v : args) | args `elem` [[], [Bool], [Function [v, v] Int]] -> Ok Void
  (Join, [Vector v, Text]) -> getResultType line AsText [v]
  _ -> failure line $ "Primitive " ++ show primitive ++ " does not accept arguments of types " ++ join ", " args

primitiveCall :: Integer -> String -> [Type] -> Fallible (Type, Primitive)
primitiveCall line name args = do
  primitive <- Map.lookup name primitives ??= failure line ("Primitive " ++ name ++ " does not exist")
  returnType <- getResultType line primitive args
  Ok (returnType, primitive)

fieldAccess :: Integer -> String -> Type -> Fallible Type
fieldAccess line name (Record fields) = Map.lookup name fields ??= failure line ("Record does not contain a field " ++ name)
fieldAccess line _ _ = failure line "Value is not a record and cannot have any fields"
