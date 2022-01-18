module Scope (Scope, Entry (..), scope, empty, add, get) where

import qualified Data.Map.Strict as Map
import qualified Language as Lang
import Fallible (Fallible (..), err)

data Entry = Constant Bool Lang.Type | Variable Lang.Type | Namespace Bool Scope deriving (Eq, Show)
data Scope = Scope String (Map.Map String Entry) deriving (Eq, Show)

scope :: String -> Map.Map String Entry -> Scope
scope = Scope

empty :: String -> Scope
empty = (`Scope` (Map.map (Constant False) Lang.constants))

getWith :: Bool -> Integer -> [String] -> Scope -> Fallible (Lang.Type, String, [String])
getWith onlyExported line (name : names) (Scope path scope) = case Map.lookup name scope of
  Just (Namespace False _) | onlyExported -> err line $ "File '" ++ path ++ "' does not export " ++ name
  Just (Constant False _) | onlyExported -> err line $ "File '" ++ path ++ "' does not export " ++ name
  Nothing -> err line $ "File '" ++ path ++ "' does not contain " ++ path
  Just (Namespace _ _) | null names -> err line $ "Identifier '" ++ name ++ "' denotes a namespace, not a value"
  Just (Namespace _ scope') -> getWith True line names scope'
  Just (Constant _ type') -> Right (type', path, names)
  Just (Variable type') -> Right (type', path, names)

get :: Integer -> [String] -> Scope -> Fallible (Lang.Type, String, [String])
get = getWith False

add :: Bool -> Integer -> String -> Entry -> Scope -> Fallible (Scope, Bool)
add assign line name entry (Scope path scope) = case Map.lookup name scope of
  Nothing -> Right (Scope path $ Map.insert name entry scope, True)
  Just previous@(Variable _) | entry == previous && assign -> Right (Scope path scope, False)
  Just _ -> err line $ "Redefinition of " ++ name
