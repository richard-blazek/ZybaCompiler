module Scope (Scope, Entry (..), empty, add, get) where

import qualified Data.Map.Strict as Map
import qualified Language as Lang
import Fallible (Fallible (..), failure)
import Functions (pair)

data Entry = Constant Lang.Type | Variable Lang.Type | Namespace Scope deriving (Eq, Show)
newtype Scope = Scope (Map.Map String Entry) deriving (Eq, Show)

empty :: Scope
empty = Scope $ Map.map Constant Lang.constants

get :: Integer -> [String] -> Scope -> Fallible Lang.Type
get line (name : names) (Scope scope) = case Map.lookup name scope of
  Just (Constant t) -> Ok t
  Just (Variable t) -> Ok t
  Just (Namespace n) | not $ null names -> get line names n
  _ -> failure line $ "Expected a variable or constant but " ++ name ++ " is a namespace"

add :: Bool -> Integer -> String -> Entry -> Scope -> Fallible (Scope, Bool)
add assign line name entry (Scope scope) = case Map.lookup name scope of
  Nothing -> Ok (Scope $ Map.insert name entry scope, True)
  Just previous@(Variable _) | entry == previous && assign -> Ok (Scope scope, False)
  Just _ -> failure line $ "Redefinition of " ++ name
