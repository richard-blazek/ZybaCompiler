module Semantics (analyse, Value (..), Statement (..), TypedValue) where

import qualified Data.Map.Strict as Map
import qualified Parser
import qualified Scope
import qualified Language as Lang
import Data.Foldable (foldlM)
import Functions (mapCatFoldlM, tailRecM, fmap2, (??))
import Fallible (Fallible, err, assert, dropLeft)

data Statement
  = Value TypedValue
  | Initialization String TypedValue
  | Assignment String TypedValue
  | While TypedValue [Statement]
  | IfChain [(TypedValue, [Statement])] [Statement] deriving (Eq, Show)

data Value
  = Literal Parser.Literal
  | Record (Map.Map String TypedValue)
  | Lambda [(String, Lang.Type)] [Statement]
  | Name String String
  | Call TypedValue [TypedValue]
  | Builtin Lang.Builtin [TypedValue]
  | Access TypedValue String
  | PhpValue deriving (Eq, Show)

type TypedValue = (Value, Lang.Type)

analyseType :: Scope.Scope -> (Integer, Parser.Value) -> Fallible Lang.Type
analyseType scope value = fmap snd $ analyseValue scope value

collectGlobal :: Scope.Scope -> (Integer, Bool, Parser.Declaration) -> Fallible Scope.Scope
collectGlobal scope (_, export, Parser.Declaration name (line, Parser.Lambda args returned body)) = do
  argTypes <- mapM (analyseType scope . snd) args
  returnType <- analyseType scope returned
  fmap fst $ Scope.add False line name (Scope.Constant export $ Lang.Function argTypes returnType) scope
collectGlobal scope _ = Right scope

collectGlobals :: String -> [(Integer, Bool, Parser.Declaration)] -> Fallible Scope.Scope
collectGlobals = foldlM collectGlobal . Scope.empty

analyseCall :: Integer -> [TypedValue] -> TypedValue -> Fallible TypedValue
analyseCall line args callee@(_, type') = case type' of
  Lang.Function from to | from == types -> Right (Call callee args, to)
  Lang.Function from to -> err line $ "Expected arguments' types " ++ show from ++ " but got " ++ show types
  _ -> err line $ "Expected a function but got " ++ show type'
  where types = map snd args

analyseBuiltin :: Scope.Scope -> (Integer, Parser.Value) -> [TypedValue] -> Maybe TypedValue
analyseBuiltin scope (line, Parser.Name names@(_ : _ : _)) args = do
  let (builtin, obj) = (last names, init names)
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

analyseValue scope (line, Parser.Name names@(name : _)) = do
  (type', path, remaining) <- Scope.get line names scope
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
  innerScope <- foldlM (\scope' (name, type') -> fmap fst $ Scope.add False line name (Scope.Variable type') scope') scope args'
  block' <- analyseBlock innerScope block
  case reverse block' of
    Value (_, type') : _ | returnType' `elem` [type', Lang.Void] -> Right (Lambda args' block', Lang.Function argTypes returnType')
    _ -> err line $ "Function must return a value of type " ++ show returnType'

analyseValue scope (line, Parser.Record fields) = do
  fields' <- mapM (analyseValue scope) fields
  Right (Record $ fields', Lang.Record $ Map.map snd fields')

analyseStatement :: Scope.Scope -> (Integer, Parser.Statement) -> Fallible (Statement, Scope.Scope)
analyseStatement scope (line, Parser.Value value) = fmap (flip (,) scope . Value) $ analyseValue scope value

analyseStatement scope (line, Parser.Assignment name value) = do
  value'@(_, type') <- analyseValue scope value
  (scope', added) <- Scope.add True line name (Scope.Variable type') scope
  Right ((if added then Initialization else Assignment) name value', scope')

analyseStatement scope (line, Parser.IfChain if' else') = do
  if'' <- mapM (\(cond, block) -> analyseValue scope cond >>= (\cond' -> fmap ((,) cond') $ analyseBlock scope block)) if'
  else'' <- analyseBlock scope else'
  assert (all ((== Lang.Bool) . snd . fst) if'') line $ "Condition must have a bool type"
  Right (IfChain if'' else'', scope)

analyseStatement scope (line, Parser.While condition body) = do
  cond <- analyseValue scope condition
  body <- analyseBlock scope body
  Right (While cond body, scope)

analyseBlock :: Scope.Scope -> [(Integer, Parser.Statement)] -> Fallible [Statement]
analyseBlock scope statements = tailRecM if' then' else' ([], scope, statements)
  where if' (_, _, statements) = Right $ null statements
        then' (result, _, _) = Right $ reverse result
        else' (result, scope, statement : statements) = fmap (\(analysed, sc) -> (analysed : result, sc, statements)) $ analyseStatement scope statement

analyseDeclaration :: Map.Map String Scope.Scope -> Scope.Scope -> (Integer, Bool, Parser.Declaration) -> Fallible (Scope.Scope, [(String, TypedValue)])
analyseDeclaration files scope (line, export, Parser.Import name path) = fmap (flip (,) [] . fst) $ Scope.add False line name (Scope.Namespace export $ files Map.! path) scope
analyseDeclaration _ scope (_, export, Parser.Declaration name value@(_, Parser.Lambda _ _ _)) = fmap ((,) scope . (:[]) . (,) name) $ analyseValue scope value
analyseDeclaration _ scope (line, export, Parser.Declaration name value) = do
  value'@(_, type') <- analyseValue scope value
  scope' <- fmap fst $ Scope.add False line name (Scope.Constant export type') scope
  Right (scope', [(name, value')])

analyseDeclaration files scope (line, export, Parser.Php name path imported) = do
  imported' <- mapM (fmap (Scope.Constant True . snd) . analyseValue scope) imported
  (scope', _) <- Scope.add False line name (Scope.Namespace export $ Scope.scope path imported') scope
  Right (scope', [])

analyse :: Map.Map String Scope.Scope -> String -> Parser.File -> Fallible (Scope.Scope, [(String, TypedValue)])
analyse files path (Parser.File declarations) = do
  globalScope <- collectGlobals path declarations
  mapCatFoldlM (analyseDeclaration files) globalScope declarations
