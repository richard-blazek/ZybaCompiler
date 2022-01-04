module Scope (Scope, empty, addConstant, addVariable, getType) where

import qualified Data.Map.Strict as Map
import qualified Language as Lang
import Fallible (Fallible (..), failure)
import Functions (pair)

data Scope = Scope (Map.Map String (Lang.Type, Bool))

empty :: Scope
empty = Scope $ Map.map (`pair` False) Lang.constants

getType :: Integer -> String -> Scope -> Fallible Lang.Type
getType line name (Scope scope) = case Map.lookup name scope of
  Just (t, _) -> Ok t
  Nothing -> failure line $ "Identifier " ++ name ++ " does not denote anything"

addIdentifier :: Bool -> Bool -> Integer -> String -> Lang.Type -> Scope -> Fallible (Scope, Bool)
addIdentifier isVariable redefine line name t (Scope scope) = case Map.lookup name scope of
  Just (_, wasVariable) | not redefine || not wasVariable -> failure line $ "Redefinition of " ++ name
  Just (previous, True) | t /= previous -> failure line $ "Assigning to " ++ name ++ " which is of type " ++ show previous ++ ", not " ++ show t
  Just (_, True) -> Ok (Scope scope, False)
  Nothing -> Ok (Scope $ Map.insert name (t, isVariable) scope, True)

addConstant :: Integer -> String -> Lang.Type -> Scope -> Fallible Scope
addConstant line name t = fmap fst . addIdentifier False False line name t

addVariable :: Bool -> Integer -> String -> Lang.Type -> Scope -> Fallible (Scope, Bool)
addVariable = addIdentifier True
