module Scope (Scope, Entry (..), scope, empty, add, get) where

import qualified Data.Map.Strict as Map
import qualified Language as Lang
import Fallible (Fallible (..), err)
import Functions (pair, (??))

import Debug.Trace (trace)

data Entry = Constant Lang.Type | Variable Lang.Type | Namespace Scope deriving (Eq, Show)
data Scope = Scope String (Map.Map String Entry) deriving (Eq, Show)

empty :: String -> Scope
empty = (`Scope` (Map.map Constant Lang.constants))

scope :: String -> Map.Map String Entry -> Scope
scope = Scope

get :: Integer -> [String] -> Scope -> Fallible (Lang.Type, String, [String])
get line (name : names) (Scope path scope) = case Map.lookup name scope of
  Just (Namespace scope') | null names -> err line "Expected a value but got a namespace"
  Just (Namespace scope') -> get line names scope'
  Just (Constant type') -> return (type', path, names)
  Just (Variable type') -> return (type', path, names)
  Nothing -> err line $ "Identifier " ++ name ++ " does not denote anything in file " ++ path

add :: Bool -> Integer -> String -> Entry -> Scope -> Fallible (Scope, Bool)
add assign line name entry (Scope path scope) = case Map.lookup name scope of
  Nothing -> Right (Scope path $ Map.insert name entry scope, True)
  Just previous@(Variable _) | entry == previous && assign -> Right (Scope path scope, False)
  Just _ -> err line $ "Redefinition of " ++ name
