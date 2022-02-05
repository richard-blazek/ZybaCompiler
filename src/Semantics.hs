module Semantics (analyse, Value (..), Statement (..), TypedValue, Declaration) where

import qualified Data.Map.Strict as Map
import qualified Parser
import qualified Scope
import qualified Language as Lang
import Data.Foldable (foldlM)
import Functions (mapCatFoldlM, tailRecM, fmap2, map2, (??), sequence2)
import Fallible (Fallible, err, assert, dropLeft)

data Statement
  = Value TypedValue
  | Initialization String TypedValue
  | Assignment String TypedValue
  | IfChain [(TypedValue, [Statement])] [Statement]
  | While TypedValue [Statement]
  | For String TypedValue [Statement]
  | Foreach String (Maybe String) TypedValue [Statement]
  | Return TypedValue deriving (Eq, Show)

data Value
  = Literal Parser.Literal
  | Record (Map.Map String TypedValue)
  | Lambda [(String, Lang.Type)] [Statement]
  | Name String String
  | Call TypedValue [TypedValue]
  | Builtin Lang.Builtin [TypedValue]
  | Access TypedValue String
  | PhpValue String deriving (Eq, Show)

type TypedValue = (Value, Lang.Type)
type Declaration = (String, TypedValue)

analyseType :: Scope.Scope -> (Integer, Parser.Value) -> Fallible Lang.Type
analyseType scope value = fmap snd $ analyseValue scope value

collectGlobal :: Scope.Scope -> (Integer, Parser.Visibility, Parser.Declaration) -> Fallible Scope.Scope
collectGlobal scope (_, export, Parser.Declaration name (line, Parser.Lambda args returned body)) = do
  argTypes <- mapM (analyseType scope . snd) args
  returnType <- analyseType scope returned
  Scope.addGlobal line export name (Lang.Function argTypes returnType) scope
collectGlobal scope _ = Right scope

collectGlobals :: String -> [(Integer, Parser.Visibility, Parser.Declaration)] -> Fallible Scope.Scope
collectGlobals = foldlM collectGlobal . Scope.empty

analyseCall :: Integer -> [TypedValue] -> TypedValue -> Fallible TypedValue
analyseCall line args callee@(_, type') = case type' of
  Lang.Function from to | from == types -> Right (Call callee args, to)
  Lang.Function from to -> err line $ "Expected arguments' types " ++ show from ++ " but got " ++ show types
  _ -> err line $ "Expected a function but got " ++ show type'
  where types = map snd args

analyseBuiltin :: Scope.Scope -> (Integer, Parser.Value) -> [TypedValue] -> Maybe TypedValue
analyseBuiltin scope (line, Parser.Name (builtin : obj@(_ : _))) args = do
  obj' <- dropLeft $ analyseValue scope (line, Parser.Name obj)
  (builtin', type') <- dropLeft $ Lang.builtinCall line builtin $ map snd (obj' : args)
  Just (Builtin builtin' (obj' : args), type')

analyseBuiltin scope (line, Parser.Access obj builtin) args = do
  obj' <- dropLeft $ analyseValue scope obj
  (builtin', type') <- dropLeft $ Lang.builtinCall line builtin $ map snd (obj' : args)
  Just (Builtin builtin' (obj' : args), type')
analyseBuiltin _ _ _ = Nothing

analyseAccess :: Integer -> Scope.Scope -> [String] -> TypedValue -> Fallible TypedValue
analyseAccess line scope [] obj = Right obj
analyseAccess line scope (name : names) obj@(_, type') = case Lang.builtinCall line name [type'] of
  Right (builtin, resultType) -> Right (Builtin builtin [obj], resultType)
  _ -> Lang.fieldAccess line name type' >>= analyseAccess line scope names . (,) (Access obj name)

analyseValue :: Scope.Scope -> (Integer, Parser.Value) -> Fallible TypedValue
analyseValue scope (_, Parser.Literal lit@(Parser.Int _)) = Right (Literal lit, Lang.Int)
analyseValue scope (_, Parser.Literal lit@(Parser.Real _)) = Right (Literal lit, Lang.Real)
analyseValue scope (_, Parser.Literal lit@(Parser.Text _)) = Right (Literal lit, Lang.Text)
analyseValue scope (_, Parser.Literal lit@(Parser.Bool _)) = Right (Literal lit, Lang.Bool)
analyseValue scope (line, Parser.Access obj name) = analyseValue scope obj >>= analyseAccess line scope [name]

analyseValue scope (line, Parser.Name names) = do
  (type', name, path, remaining) <- Scope.get line (reverse names) scope
  analyseAccess line scope remaining (Name path name, type')

analyseValue scope (line, Parser.Call callee args) = do
  args' <- mapM (analyseValue scope) args
  analyseBuiltin scope callee args' ?? (analyseValue scope callee >>= analyseCall line args')

analyseValue scope (line, Parser.Operation name args) = do
  args' <- mapM (analyseValue scope) args
  (builtin, type') <- Lang.builtinCall line name $ map snd args'
  Right (Builtin builtin args', type')

analyseValue scope (line, Parser.Lambda args returnType block) = do
  argTypes <- mapM (analyseType scope . snd) args
  let args' = zip (map fst args) argTypes
  returnType' <- analyseType scope returnType
  innerScope <- foldlM (\scope' (name, type') -> Scope.addVariable line name type' scope') scope args'
  block' <- analyseBlock innerScope returnType' block
  let fn = (Lambda args' block', Lang.Function argTypes returnType')
  case reverse block' of
    Return _ : _ -> Right fn
    Value (_, type') : _ | type' == returnType' -> Right fn
    _ | returnType' == Lang.Void -> Right fn
    _ -> err line $ "Function must return a value of type " ++ show returnType'

analyseValue scope (line, Parser.Record fields) = do
  fields' <- mapM (analyseValue scope) fields
  Right (Record $ fields', Lang.Record $ Map.map snd fields')

analyseStatement :: Scope.Scope -> Lang.Type -> (Integer, Parser.Statement) -> Fallible (Statement, Scope.Scope)
analyseStatement scope returnType (line, Parser.Value value) = fmap (flip (,) scope . Value) $ analyseValue scope value
analyseStatement scope returnType (line, Parser.Assignment name value) = do
  value'@(_, type') <- analyseValue scope value
  (scope', added) <- Scope.setVariable line name type' scope
  Right ((if added then Initialization else Assignment) name value', scope')

analyseStatement scope returnType (line, Parser.IfChain if' else') = do
  if'' <- mapM (\(cond, block) -> sequence2 (analyseValue scope) (analyseBlock scope returnType) cond block >>= uncurry checkCond) if'
  else'' <- analyseBlock scope returnType else'
  Right (IfChain if'' else'', scope)
  where checkCond cond@(_, type') block = assert (type' == Lang.Bool) line "Condition must have a bool type" >> return (cond, block)

analyseStatement scope returnType (line, Parser.While cond body) = do
  cond'@(_, type') <- analyseValue scope cond
  assert (type' == Lang.Bool) line $ "Condition must have a bool type"
  body' <- analyseBlock scope returnType body
  Right (While cond' body', scope)

analyseStatement scope returnType (line, Parser.For value count body) = do
  count'@(_, type') <- analyseValue scope count
  assert (type' == Lang.Int) line $ "Number of repetitions must have an int type"
  innerScope <- Scope.addConstant line value Lang.Int scope
  body' <- analyseBlock innerScope returnType body
  Right (For value count' body', scope)

analyseStatement scope returnType (line, Parser.Foreach valueName keyName iterable body) = do
  iterable'@(_, type') <- analyseValue scope iterable
  iteratingVariables <- variables type' keyName
  innerScope <- foldlM (\scope' (name, type') -> Scope.addVariable line name type' scope') scope iteratingVariables
  body' <- analyseBlock innerScope returnType body
  Right (Foreach valueName keyName iterable' body', scope)
  where variables (Lang.Vector valueType) (Just keyName) = Right [(valueName, valueType), (keyName, Lang.Int)]
        variables (Lang.Vector valueType) Nothing = Right [(valueName, valueType)]
        variables (Lang.Dictionary keyType valueType) (Just keyName) = Right [(valueName, valueType), (keyName, keyType)]
        variables (Lang.Dictionary _ valueType) Nothing = Right [(valueName, valueType)]
        variables _ _ = err line "Iterable must be a vector or a dictionary"

analyseStatement scope returnType (line, Parser.Return value) = do
  value'@(_, type') <- analyseValue scope value
  assert (type' == returnType) line $ "Return value must have type " ++ show returnType
  Right (Return value', scope)

analyseBlock :: Scope.Scope -> Lang.Type -> [(Integer, Parser.Statement)] -> Fallible [Statement]
analyseBlock scope returnType statements = tailRecM if' then' else' ([], scope, statements)
  where if' (_, _, statements) = Right $ null statements
        then' (result, _, _) = Right $ reverse result
        else' (result, scope, statement : statements) = fmap (\(analysed, sc) -> (analysed : result, sc, statements)) $ analyseStatement scope returnType statement

analyseDeclaration :: Map.Map String Scope.Scope -> Scope.Scope -> (Integer, Parser.Visibility, Parser.Declaration) -> Fallible (Scope.Scope, [Declaration])
analyseDeclaration files scope (line, export, Parser.Import name path) = fmap (flip (,) []) $ Scope.addNamespace line export name (files Map.! path) scope
analyseDeclaration _ scope (_, export, Parser.Declaration name value@(_, Parser.Lambda _ _ _)) = fmap ((,) scope . (:[]) . (,) name) $ analyseValue scope value
analyseDeclaration _ scope (line, export, Parser.Declaration name value) = do
  value'@(_, type') <- analyseValue scope value
  scope' <- Scope.addGlobal line export name type' scope
  Right (scope', [(name, value')])

analyseDeclaration files scope (line, export, Parser.Php name path imported) = do
  imported' <- fmap Map.toList $ mapM (fmap snd . analyseValue scope) imported
  importedScope <- foldlM (\scope (name, type') -> Scope.addGlobal line Parser.Exported name type' scope) (Scope.empty path) imported'
  scope' <- Scope.addNamespace line export name importedScope scope
  Right (scope', map (\(k, t) -> (k, (PhpValue k, t))) imported')

analyse :: Map.Map String Scope.Scope -> String -> Parser.File -> Fallible (Scope.Scope, [Declaration])
analyse files path (Parser.File declarations) = do
  globalScope <- collectGlobals path declarations
  mapCatFoldlM (analyseDeclaration files) globalScope declarations
