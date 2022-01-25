module Scope (Scope, Visibility (..), Mutability (..), scope, empty, addGlobal, addNamespace, addLocal, setLocal, get) where

import qualified Data.Map.Strict as Map
import qualified Language as Lang
import Fallible (Fallible (..), err)

data Visibility = Exported | Private deriving (Eq, Show)
data Mutability = Constant | Variable deriving (Eq, Show)
data Entry = Local Mutability Lang.Type | Global Visibility Lang.Type | Namespace Visibility Scope deriving (Eq, Show)
data Scope = Scope String (Map.Map String Entry) deriving (Eq, Show)

scope :: String -> Map.Map String Entry -> Scope
scope = Scope

empty :: String -> Scope
empty = (`Scope` Map.map (Global Private) Lang.constants)

getWith :: Bool -> Integer -> [String] -> Scope -> Fallible (Lang.Type, String, [String])
getWith onlyExported line (name : names) (Scope path scope) = case Map.lookup name scope of
  Just (Namespace Private _) | onlyExported -> err line $ "File '" ++ path ++ "' does not export " ++ name
  Just (Global Private _) | onlyExported -> err line $ "File '" ++ path ++ "' does not export " ++ name
  Nothing -> err line $ "File '" ++ path ++ "' does not contain " ++ path
  Just (Namespace _ _) | null names -> err line $ "Identifier '" ++ name ++ "' denotes a namespace, not a value"
  Just (Namespace _ scope') -> getWith True line names scope'
  Just (Global _ type') -> Right (type', path, names)
  Just (Local _ type') -> Right (type', path, names)

get :: Integer -> [String] -> Scope -> Fallible (Lang.Type, String, [String])
get = getWith False

add :: Integer -> Bool -> String -> Entry -> Scope -> Fallible (Scope, Bool)
add line assign name entry (Scope path scope) = case Map.lookup name scope of
  Nothing -> Right (Scope path $ Map.insert name entry scope, True)
  Just previous@(Local Variable _) | entry == previous && assign -> Right (Scope path scope, False)
  Just _ -> err line $ "Redefinition of " ++ name

addGlobal :: Integer -> Visibility -> String -> Lang.Type -> Scope -> Fallible Scope
addGlobal line visibility name type' = fmap fst . add line False name (Global visibility type')

addNamespace :: Integer -> Visibility -> String -> Scope -> Scope -> Fallible Scope
addNamespace line visibility name subscope = fmap fst . add line False name (Namespace visibility subscope)

addLocal :: Integer -> Mutability -> String -> Lang.Type -> Scope -> Fallible Scope
addLocal line mutability name type' = fmap fst . add line False name (Local mutability type')

setLocal :: Integer -> Mutability -> String -> Lang.Type -> Scope -> Fallible (Scope, Bool)
setLocal line mutability name type' = add line True name (Local mutability type')
