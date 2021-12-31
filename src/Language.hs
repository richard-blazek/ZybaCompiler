module Language (Type (..), Primitive (..), primitiveCall, fieldAccess, removePrimitives, constants) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Errors (Fallible, failure, assert)
import Functions (join, split, (??))

data Type = Void | Int | Bool | Float | Text | Function [Type] Type | MapArray Type Type | IntArray Type | Record (Map.Map String Type) deriving (Eq, Show)
data Primitive = Add | Sub | Mul | Div | IntDiv | Rem | And | Or | Xor | Eq | Neq | Lt | Gt | Le | Ge | Pow | Not
  | AsInt | AsFloat | AsBool | AsText | Fun | Map | Array | Set | Get | Has | Size deriving (Eq, Ord)

instance Show Primitive where
  show primitive = primitivesReversed Map.! primitive
    where primitivesReversed = Map.fromList $ map (\(k, v) -> (v, k)) $ Map.toList primitives

primitives :: Map.Map String Primitive
primitives = Map.fromList [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("//", IntDiv), ("%", Rem), ("&", And), ("|", Or), ("^", Xor), ("==", Eq), ("!=", Neq),
  ("<", Lt), (">", Gt), ("<=", Le), (">=", Ge), ("**", Pow), ("not", Not), ("asInt", AsInt), ("asFloat", AsFloat), ("asBool", AsBool), ("asText", AsText),
  ("fun", Fun), ("map", Map), ("array", Array), ("set", Set), ("get", Get), ("has", Has), ("size", Size)]

primitivesSet :: Set.Set String
primitivesSet = Map.keysSet primitives

constants :: Map.Map String Type
constants = Map.fromList [("int", Int), ("bool", Bool), ("float", Float), ("text", Text), ("void", Void)]

removePrimitives :: Set.Set String -> Set.Set String
removePrimitives = flip Set.difference primitivesSet

isKindOf :: Type -> Type -> Bool
isKindOf a b | a == b = True
isKindOf a Void = True
isKindOf (IntArray v1) (IntArray v2) = isKindOf v1 v2
isKindOf (MapArray k1 v1) (MapArray k2 v2) = k1 == k2 && isKindOf v1 v2
isKindOf (Record f1) (Record f2) = null (Map.filterWithKey (\k2 v2 -> fmap (`isKindOf` v2) (Map.lookup k2 f1) /= Just True) f2)
isKindOf _ _ = False

getResultType :: Integer -> Primitive -> [Type] -> Fallible Type
getResultType line primitive args = case (primitive, args) of
  (Add, [Int, Int]) -> Right Int
  (Add, [a, b]) | all (`elem` [Int, Float]) [a, b] -> Right Float
  (Add, [Text, Text]) -> Right Text
  (Add, [IntArray v1, IntArray v2]) | v1 == v2 -> Right $ IntArray v1
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
  (Or, [IntArray v1, IntArray v2]) | v1 == v2 -> Right $ IntArray v1
  (Or, [MapArray k1 v1, MapArray k2 v2]) | v1 == v2 && k1 == k2 -> Right $ MapArray k1 v1
  (Xor, [Int, Int]) -> Right Int
  (Xor, [Bool, Bool]) -> Right Bool
  (Eq, [Int, Int]) -> Right Bool
  (Eq, [Bool, Bool]) -> Right Bool
  (Eq, [Text, Text]) -> Right Bool
  (Eq, [IntArray v1, IntArray v2]) -> do
    getResultType line Eq [v1, v2]
    Right Bool
  (Eq, [MapArray k1 v1, MapArray k2 v2]) -> do
    getResultType line Eq [k1, k2]
    getResultType line Eq [v1, v2]
    Right Bool
  (Eq, [a, b]) | any (== Float) [a, b] -> failure line "Do not compare floats for equality. Learn more: https://stackoverflow.com/questions/1088216/whats-wrong-with-using-to-compare-floats-in-java"
  (Neq, [Int, Int]) -> Right Bool
  (Neq, [Bool, Bool]) -> Right Bool
  (Neq, [Text, Text]) -> Right Bool
  (Neq, [IntArray v1, IntArray v2]) -> getResultType line Eq [IntArray v1, IntArray v2]
  (Neq, [MapArray k1 v1, MapArray k2 v2]) -> getResultType line Eq [MapArray k1 v1, MapArray k2 v2]
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
  (Array, value : args) -> do
    assert (all (== value) args) line "All values must have the specified type"
    Right $ IntArray value
  (Map, key : value : args) -> do
    assert (key `elem` [Int, Text]) line "Map keys must be either int or text"
    let pairs = split args
    assert (pairs /= Nothing) line "Missing value for the last key"
    assert (fmap (all (== (key, value))) pairs == Just True) line "All keys and values must have the specified type"
    Right $ MapArray key value
  (Get, [IntArray value, Int]) -> Right value
  (Get, [MapArray key value, index]) | index == key -> Right value
  (Set, [IntArray value, Int, assigned]) | assigned == value -> Right Void
  (Set, [MapArray key value, index, assigned]) | index == key && assigned == value -> Right Void
  (Has, [IntArray value, Int]) -> Right Bool
  (Has, [MapArray key value, index]) | index == key -> Right Bool
  (Size, [Text]) -> Right Int
  (Size, [IntArray _]) -> Right Int
  (Size, [MapArray _ _]) -> Right Int
  _ -> failure line $ "Primitive " ++ show primitive ++ " does not accept arguments of types " ++ join ", " args

primitiveCall :: Integer -> String -> [Type] -> Fallible (Type, Primitive)
primitiveCall line name args = do
  primitive <- fmap Right (Map.lookup name primitives) ?? failure line ("Primitive " ++ name ++ " does not exist")
  returnType <- getResultType line primitive args
  Right (returnType, primitive)

fieldAccess :: Integer -> String -> Type -> Fallible Type
fieldAccess line name (Record fields) = fmap Right (Map.lookup name fields) ?? failure line ("Record does not contain a field " ++ name)
fieldAccess line _ _ = failure line "Value is not a record and cannot have any fields"
