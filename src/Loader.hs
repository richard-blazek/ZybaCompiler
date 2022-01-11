module Loader (load) where

import qualified Data.Map as Map
import qualified Data.Tree as Tree
import qualified Lexer
import qualified Scope
import qualified Parser
import qualified Semantics
import qualified Language as Lang
import Fallible (Fallible, FallibleT, success, failure, wrap)
import Data.Maybe (catMaybes)
import Data.Containers.ListUtils (nubOrdOn)
import Functions (mapCatFoldlM, map2, fmap2, leaf)

data File = Zyba Parser.File | Php String
data Lang = ZybaLang | PhpLang deriving (Eq, Show)
data LoadedFile = LoadedFile File [String] [String]
type Cache = Map.Map String LoadedFile
type Dependencies = Map.Map String (Tree.Tree (String, File))

loadFile :: Lang -> String -> FallibleT IO LoadedFile
loadFile PhpLang path = success (readFile path) >>= \content -> return (LoadedFile (Php content) [] [])
loadFile ZybaLang path = do
  content <- success $ readFile path
  parsed@(Parser.File declarations) <- wrap $ Parser.parse $ Lexer.tokenize content
  let (zybas, phps) = map2 catMaybes catMaybes $ unzip $ map import' declarations
  return $ LoadedFile (Zyba parsed) zybas phps
  where import' (_, Parser.Import _ imported) = (Just imported, Nothing)
        import' (_, Parser.Php imported _) = (Nothing, Just imported)
        import' _ = (Nothing, Nothing)

lookupCache :: Lang -> Cache -> String -> FallibleT IO (Cache, LoadedFile)
lookupCache lang cache path = case Map.lookup path cache of
  Just file@(LoadedFile (Zyba _) _ _) | lang == ZybaLang -> return (cache, file)
  Just file@(LoadedFile (Php _) _ _) | lang == PhpLang -> return (cache, file)
  _ -> do
    file <- loadFile lang path
    return (Map.insert path file cache, file)

loadDependencies :: Cache -> [String] -> Dependencies -> FallibleT IO Dependencies
loadDependencies _ [] deps = return deps
loadDependencies cache (path : upper) deps = do
  (cache, LoadedFile file zybas phps) <- lookupCache ZybaLang cache path
  (cache, phpContents) <- getPhps cache phps
  let children = map (deps Map.!) zybas ++ zipWith (curry leaf) phps phpContents
  case filter (`Map.notMember` deps) zybas of
    [] -> loadDependencies cache upper $ Map.insert path (Tree.Node (path, file) children) deps
    next : _ -> checkCircular next >> loadDependencies cache (next : path : upper) deps
  where getPhps = mapCatFoldlM (\cache' path' -> fmap2 id ((:[]) . getFile) $ lookupCache PhpLang cache' path')
        getFile (LoadedFile file _ _) = file
        checkCircular path = if path `elem` upper then wrap $ failure (-1) $ "Circular dependency: " ++ path else return ()

orderDependencies :: Ord a => Tree.Tree (a, b) -> [(a, b)]
orderDependencies = nubOrdOn fst . concat . reverse . Tree.levels

listOfImportedFiles :: String -> FallibleT IO [(String, File)]
listOfImportedFiles path = do
  deps <- loadDependencies Map.empty [path] Map.empty 
  return $ orderDependencies $ deps Map.! path

type Declarations = (String, [(String, (Lang.Type, Semantics.Expression))])
analyseAll :: Map.Map String Scope.Scope -> [String] -> [Declarations] -> [(String, File)] -> Fallible ([String], [Declarations])
analyseAll _ phps zybas [] = return $ (reverse phps, reverse zybas)
analyseAll known phps zybas ((_, Php content) : paths) = analyseAll known (content : phps) zybas paths
analyseAll known phps zybas ((path, Zyba parsed) : paths) = do
  (scope, declarations) <- Semantics.analyse known path parsed
  analyseAll (Map.insert path scope known) phps ((path, declarations) : zybas) paths

load :: String -> FallibleT IO ([String], [Declarations])
load path = listOfImportedFiles path >>= wrap . analyseAll Map.empty [] []
