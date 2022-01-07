module Loader (load) where

import qualified Data.Map as Map
import qualified Data.Tree as Tree
import qualified Lexer
import qualified Parser
import Fallible (FallibleT, success, failure, wrap)
import Data.Maybe (mapMaybe)
import Data.Containers.ListUtils (nubOrdOn)

type FallibleIO = FallibleT IO
type Cache = Map.Map String ([String], Parser.File)
type Dependencies = Map.Map String (Tree.Tree (String, Parser.File))

loadFile :: String -> FallibleIO ([String], Parser.File)
loadFile path = do
  content <- success $ readFile path
  file@(Parser.File declarations) <- wrap $ Parser.parse $ Lexer.tokenize content
  return (mapMaybe import' declarations, file)
  where import' (_, Parser.Import _ imported) = Just imported
        import' _ = Nothing

lookupCache :: String -> Cache -> FallibleIO ([String], Parser.File, Cache)
lookupCache path cache = case Map.lookup path cache of
  Just (imports, file) -> return (imports, file, cache)
  Nothing -> do
    (imports, file) <- loadFile path
    return (imports, file, Map.insert path (imports, file) cache)

loadDependencies :: Cache -> [String] -> Dependencies -> FallibleIO Dependencies
loadDependencies _ [] deps = return deps
loadDependencies cache (path : upper) deps = do
  (imports, file, cache') <- lookupCache path cache
  case filter (`Map.notMember` deps) imports of
    [] -> loadDependencies cache' upper $ Map.insert path (Tree.Node (path, file) $ map (deps Map.!) imports) deps
    x : _ -> if x `elem` upper
              then wrap $ failure (-1) $ "Circular dependency: " ++ x
              else loadDependencies cache' (x : path : upper) deps

orderDependencies :: Ord a => Tree.Tree (a, b) -> [(a, b)]
orderDependencies = nubOrdOn fst . concat . reverse . Tree.levels

load :: String -> FallibleIO [(String, Parser.File)]
load path = do
  deps <- loadDependencies Map.empty [path] Map.empty 
  return $ orderDependencies $ deps Map.! path
