module Scope (Scope, empty, addConstant, addVariable, getType) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Language as Lang
import Errors (Fallible, failure)

data Scope = Scope (Map.Map String (Lang.Type, Bool))

empty :: Scope
empty = Scope Map.empty

getType :: Integer -> String -> Scope -> Fallible Lang.Type
getType line name (Scope scope) = case Map.lookup name scope of
  Just (type', _) -> Right type'
  Nothing | Lang.isPrimitive name -> failure line $ "Identifier " ++ name ++ " denotes a primitive"
  Nothing -> failure line $ "Identifier " ++ name ++ " does not denote anything at all"

addConstant :: Integer -> String -> Lang.Type -> Scope -> Fallible Scope
addConstant line name type' (Scope scope)
  | Lang.isPrimitive name || Map.member name scope = failure line $ "Redefinition of " ++ name
  | otherwise = Right $ Scope $ Map.insert name (type', False) scope

addVariable :: Integer -> String -> Lang.Type -> Scope -> Fallible (Scope, Bool)
addVariable line name type' (Scope scope) = case Map.lookup name scope of
  Nothing -> Right (Scope $ Map.insert name (type', True) scope, True)
  Just (_, False) -> failure line $ "Assigning to " ++ show name ++ " which is a constant"
  Just (previousType, True) | type' == previousType -> Right (Scope scope, False)
  Just (previousType, True) -> failure line $ "Assigning to " ++ name ++ " which is of type " ++ show previousType ++ ", not " ++ show type'
