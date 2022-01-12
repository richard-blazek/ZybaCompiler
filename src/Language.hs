module Language (Type (..), Primitive (..), primitiveCall, fieldAccess, removePrimitives, constants) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Fallible (Fallible (..), err, assert)
import Functions (join, split, (??), zipMaps)

data Type = Void | Int | Bool | Real | Text | Function [Type] Type | Dictionary Type Type | Vector Type | Record (Map.Map String Type) deriving (Eq, Show)
data Primitive = Add | Sub | Mul | Div | IntDiv | Rem | And | Or | Xor | Eq | Neq | Lt | Gt | Le | Ge | Pow | Not
  | AsInt | AsReal | AsBool | AsText | Fun | Dict | List | Set | Get | Has | Size | Concat | Append | Sized | Sort
  | Join deriving (Eq, Ord)

instance Show Primitive where
  show primitive = primitivesReversed Map.! primitive
    where primitivesReversed = Map.fromList $ map (\(k, v) -> (v, k)) $ Map.toList primitives

primitives :: Map.Map String Primitive
primitives = Map.fromList [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("//", IntDiv), ("%", Rem), ("&", And), ("|", Or), ("^", Xor), ("==", Eq), ("!=", Neq),
  ("<", Lt), (">", Gt), ("<=", Le), (">=", Ge), ("**", Pow), ("not", Not), ("asInt", AsInt), ("asReal", AsReal), ("asBool", AsBool), ("asText", AsText),
  ("fun", Fun), ("dict", Dict), ("list", List), ("set", Set), ("get", Get), ("has", Has), ("size", Size), ("concat", Concat), ("append", Append), ("sized", Sized),
  ("sort", Sort), ("join", Join)]

primitivesSet :: Set.Set String
primitivesSet = Map.keysSet primitives

constants :: Map.Map String Type
constants = Map.fromList [("int", Int), ("bool", Bool), ("real", Real), ("text", Text), ("void", Void)]

removePrimitives :: Set.Set String -> Set.Set String
removePrimitives = (`Set.difference` primitivesSet)

getResultType :: Integer -> Primitive -> [Type] -> Fallible Type
getResultType line primitive args = case (primitive, args) of
  (Add, [Int, Int]) -> Right Int
  (Add, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Real
  (Add, [Text, Text]) -> Right Text
  (Add, [Vector v1, Vector v2]) | v1 == v2 -> Right $ Vector v1
  (Sub, [Int, Int]) -> Right Int
  (Sub, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Real
  (Mul, [Int, Int]) -> Right Int
  (Mul, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Real
  (Div, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Real
  (IntDiv, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Int
  (Rem, [Int, Int]) -> Right Int
  (Rem, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Real
  (And, [Int, Int]) -> Right Int
  (And, [Bool, Bool]) -> Right Bool
  (Or, [Int, Int]) -> Right Int
  (Or, [Bool, Bool]) -> Right Bool
  (Or, [Dictionary k1 v1, Dictionary k2 v2]) | v1 == v2 && k1 == k2 -> Right $ Dictionary k1 v1
  (Or, [Record f1, Record f2]) -> Right $ Record $ Map.union f2 f1
  (Xor, [Int, Int]) -> Right Int
  (Xor, [Bool, Bool]) -> Right Bool
  (Eq, [Int, Int]) -> Right Bool
  (Eq, [Bool, Bool]) -> Right Bool
  (Eq, [Text, Text]) -> Right Bool
  (Eq, [Vector v1, Vector v2]) -> getResultType line Eq [v1, v2]
  (Eq, [Dictionary k1 v1, Dictionary k2 v2]) -> getResultType line Eq [k1, k2] >> getResultType line Eq [v1, v2]
  (Eq, [Record f1, Record f2]) -> do
    mapM (getResultType line Eq) $ map (\(a, b) -> [a, b]) $ Map.elems $ zipMaps f1 f2
    Right Bool
  (Eq, [a, b]) | any (== Real) [a, b] -> err line "Do not compare reals for equality. Learn more: https://stackoverflow.com/questions/1088216/whats-wrong-with-using-to-compare-floats-in-java"
  (Neq, compared) -> getResultType line Eq compared
  (Lt, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Bool
  (Lt, [Text, Text]) -> Right Bool
  (Gt, compared) -> getResultType line Lt compared
  (Le, compared) -> getResultType line Lt compared
  (Ge, compared) -> getResultType line Lt compared
  (Pow, [Int, Int]) -> Right Int
  (Pow, [a, b]) | all (`elem` [Int, Real]) [a, b] -> Right Real
  (Not, [Int]) -> Right Int
  (Not, [Bool]) -> Right Bool
  (AsInt, [a]) | a `elem` [Int, Real, Bool, Text] -> Right Int
  (AsReal, [a]) | a `elem` [Int, Real, Bool, Text] -> Right Real
  (AsBool, [a]) | a `elem` [Int, Real, Bool, Text] -> Right Bool
  (AsText, [a]) | a `elem` [Int, Real, Bool, Text] -> Right Text
  (AsText, [Vector v]) -> getResultType line AsText [v]
  (AsText, [Dictionary k v]) -> getResultType line AsText [v]
  (AsText, [Record fields]) -> do
    mapM (getResultType line AsText . (:[])) $ Map.elems fields
    Right Text
  (Fun, returned : args) -> Right $ Function args returned
  (List, value : args) -> do
    assert (all (== value) args) line "All values must have the specified type"
    Right $ Vector value
  (Dict, key : value : args) -> do
    assert (key `elem` [Int, Text]) line "Map keys must be either int or text"
    let pairs = split args
    assert (pairs /= Nothing) line "Missing value for the last key"
    assert (fmap (all (== (key, value))) pairs == Just True) line "All keys and values must have the specified type"
    Right $ Dictionary key value
  (Get, [Vector value, Int]) -> Right value
  (Get, [Dictionary key value, index]) | index == key -> Right value
  (Set, [Vector value, Int, assigned]) | assigned == value -> Right Void
  (Set, [Dictionary key value, index, assigned]) | index == key && assigned == value -> Right Void
  (Has, [Dictionary key value, index]) | index == key -> Right Bool
  (Size, [Text]) -> Right Int
  (Size, [Vector _]) -> Right Int
  (Size, [Dictionary _ _]) -> Right Int
  (Concat, Vector v : args) | all (`elem` [Vector v, v]) args -> Right $ Vector v
  (Append, Vector v : args) | all (`elem` [Vector v, v]) args -> Right Void
  (Sized, [Vector v, Int]) -> Right $ Vector v
  (Sized, [Vector v, Int, v2]) | v2 == v -> Right $ Vector v
  (Sort, Vector v : args) | args `elem` [[], [Bool], [Function [v, v] Int]] -> Right Void
  (Join, [Vector v, Text]) -> getResultType line AsText [v]
  _ -> err line $ "Primitive " ++ show primitive ++ " does not accept arguments of types " ++ join ", " args

primitiveCall :: Integer -> String -> [Type] -> Fallible (Type, Primitive)
primitiveCall line name args = do
  primitive <- Map.lookup name primitives ?? err line ("Primitive " ++ name ++ " does not exist")
  returnType <- getResultType line primitive args
  Right (returnType, primitive)

fieldAccess :: Integer -> String -> Type -> Fallible Type
fieldAccess line name (Record fields) = Map.lookup name fields ?? err line ("The record does not have a field " ++ name)
fieldAccess line name _ = err line $ "It is not a record; therefore it cannot have a field " ++ name
