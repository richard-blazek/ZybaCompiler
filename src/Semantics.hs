module Semantics (analyse, Value (..), Statement (..)) where

import qualified Data.Map.Strict as Map
import qualified Parser
import qualified Scope
import qualified Language as Lang
import Data.Foldable (foldlM)
import Functions (pair, mapCatFoldlM, tailRecM, fmap2, (??))
import Fallible (Fallible, err, assert, dropLeft)

data Statement
  = Value (Lang.Type, Value)
  | Initialization String (Lang.Type, Value)
  | Assignment String (Lang.Type, Value)
  | While (Lang.Type, Value) [Statement]
  | IfChain [((Lang.Type, Value), [Statement])] [Statement] deriving (Eq, Show)

data Value
  = Literal Parser.Literal
  | Record (Map.Map String (Lang.Type, Value))
  | Lambda [(String, Lang.Type)] [Statement]
  | Name String String
  | Call (Lang.Type, Value) [(Lang.Type, Value)]
  | Primitive Lang.Primitive [(Lang.Type, Value)]
  | Access (Lang.Type, Value) String
  | PhpValue deriving (Eq, Show)

analyseType :: Scope.Scope -> (Integer, Parser.Value) -> Fallible Lang.Type
analyseType scope expr = fmap fst $ analyseValue scope expr

collectGlobal :: Scope.Scope -> (Integer, Parser.Declaration) -> Fallible Scope.Scope
collectGlobal scope (_, Parser.Declaration name (line, Parser.Lambda args returned body)) = do
  argTypes <- mapM (analyseType scope . snd) args
  returnType <- analyseType scope returned
  fmap fst $ Scope.add False line name (Scope.Constant $ Lang.Function argTypes returnType) scope
collectGlobal scope _ = Right scope

collectGlobals :: String -> [(Integer, Parser.Declaration)] -> Fallible Scope.Scope
collectGlobals = foldlM collectGlobal . Scope.empty

analyseCall :: Integer -> [(Lang.Type, Value)] -> (Lang.Type, Value) -> Fallible (Lang.Type, Value)
analyseCall line args callee@(type', _) = case type' of
  Lang.Function from to | from == types -> Right (to, Call callee args)
  Lang.Function from to -> err line $ "Expected arguments' types " ++ show from ++ " but got " ++ show types
  _ -> err line $ "Expected a function but got " ++ show type'
  where types = map fst args

analysePrimitive :: Scope.Scope -> (Integer, Parser.Value) -> [(Lang.Type, Value)] -> Maybe (Lang.Type, Value)
analysePrimitive scope (line, Parser.Name names@(_ : _ : _)) args = do
  let (primitive, obj) = (last names, init names)
  obj' <- dropLeft $ analyseValue scope (line, Parser.Name obj)
  (type', primitive') <- dropLeft $ Lang.primitiveCall line primitive $ map fst (obj' : args)
  Just (type', Primitive primitive' (obj' : args))

analysePrimitive scope (line, Parser.Access obj primitive) args = do
  obj' <- dropLeft $ analyseValue scope obj
  (type', primitive') <- dropLeft $ Lang.primitiveCall line primitive $ map fst (obj' : args)
  Just (type', Primitive primitive' (obj' : args))
analysePrimitive _ _ _ = Nothing

analyseAccess :: Integer -> Scope.Scope -> [String] -> (Lang.Type, Value) -> Fallible (Lang.Type, Value)
analyseAccess line scope [] obj = Right obj
analyseAccess line scope (name : names) obj@(type', _) = case Lang.primitiveCall line name [type'] of
  Right (resultType, primitive) -> Right (resultType, Primitive primitive [obj])
  _ -> Lang.fieldAccess line name type' >>= analyseAccess line scope names . (`pair` Access obj name)

analyseValue :: Scope.Scope -> (Integer, Parser.Value) -> Fallible (Lang.Type, Value)
analyseValue scope (_, Parser.Literal lit@(Parser.Int _)) = Right (Lang.Int, Literal lit)
analyseValue scope (_, Parser.Literal lit@(Parser.Real _)) = Right (Lang.Real, Literal lit)
analyseValue scope (_, Parser.Literal lit@(Parser.Text _)) = Right (Lang.Text, Literal lit)
analyseValue scope (_, Parser.Literal lit@(Parser.Bool _)) = Right (Lang.Bool, Literal lit)
analyseValue scope (line, Parser.Access obj name) = analyseValue scope obj >>= analyseAccess line scope [name]

analyseValue scope (line, Parser.Name names@(name : _)) = do
  (type', path, remaining) <- Scope.get line names scope
  analyseAccess line scope remaining (type', Name path name)

analyseValue scope (line, Parser.Call callee args) = do
  args' <- mapM (analyseValue scope) args
  analysePrimitive scope callee args' ?? (analyseValue scope callee >>= analyseCall line args')

analyseValue scope (line, Parser.Operation name args) = do
  args' <- mapM (analyseValue scope) args
  (type', primitive) <- Lang.primitiveCall line name $ map fst args'
  Right (type', Primitive primitive args')

analyseValue scope (line, Parser.Lambda args returnType block) = do
  argTypes <- mapM (analyseType scope . snd) args
  let args' = zip (map fst args) argTypes
  returnType' <- analyseType scope returnType
  innerScope <- foldlM (\scope (name, type') -> fmap fst $ Scope.add False line name (Scope.Variable type') scope) scope args'
  block' <- analyseBlock innerScope block
  case reverse block' of
    Value (type', _) : _ | returnType' `elem` [type', Lang.Void] -> Right (Lang.Function argTypes returnType', Lambda args' block')
    _ -> err line $ "Function must return a value of type " ++ show returnType'

analyseValue scope (line, Parser.Record fields) = do
  fields' <- mapM (analyseValue scope) fields
  Right (Lang.Record $ Map.map fst fields', Record $ fields')

analyseStatement :: Scope.Scope -> (Integer, Parser.Statement) -> Fallible (Statement, Scope.Scope)
analyseStatement scope (line, Parser.Value expr) = fmap ((`pair` scope) . Value) $ analyseValue scope expr

analyseStatement scope (line, Parser.Assignment name expr) = do
  value@(type', _) <- analyseValue scope expr
  (scope', added) <- Scope.add True line name (Scope.Variable type') scope
  Right . (`pair` scope') $ (if added then Initialization else Assignment) name value

analyseStatement scope (line, Parser.IfChain ifs else') = do
  ifChain <- mapM (\(cond, block) -> analyseValue scope cond >>= (\cond' -> fmap (pair cond') $ analyseBlock scope block)) ifs
  elseBlock <- analyseBlock scope else'
  assert (all ((== Lang.Bool) . fst . fst) ifChain) line $ "Condition must have a bool type"
  Right (IfChain ifChain elseBlock, scope)

analyseStatement scope (line, Parser.While condition body) = do
  cond <- analyseValue scope condition
  body <- analyseBlock scope body
  Right (While cond body, scope)

analyseBlock :: Scope.Scope -> [(Integer, Parser.Statement)] -> Fallible [Statement]
analyseBlock scope statements = tailRecM if' then' else' ([], scope, statements)
  where if' (_, _, statements) = Right $ null statements
        then' (result, _, _) = Right $ reverse result
        else' (result, scope, statement : statements) = fmap (\(analysed, sc) -> (analysed : result, sc, statements)) $ analyseStatement scope statement

analyseDeclaration :: Map.Map String Scope.Scope -> Scope.Scope -> (Integer, Parser.Declaration) -> Fallible (Scope.Scope, [(String, (Lang.Type, Value))])
analyseDeclaration files scope (line, Parser.Import name path) = fmap ((`pair` []) . fst) $ Scope.add False line name (Scope.Namespace $ files Map.! path) scope
analyseDeclaration _ scope (_, Parser.Declaration name expression@(_, Parser.Lambda _ _ _)) = fmap (pair scope . (:[]) . pair name) $ analyseValue scope expression
analyseDeclaration _ scope (line, Parser.Declaration name expression) = do
  expression@(type', _) <- analyseValue scope expression
  scope' <- fmap fst $ Scope.add False line name (Scope.Constant type') scope
  Right (scope', [(name, expression)])

analyseDeclaration files scope (line, Parser.Php _ imported) = do
  imported' <- mapM (fmap fst . analyseValue scope) imported
  scope' <- foldlM (\scope (name, type') -> fmap fst $ Scope.add False line name (Scope.Constant type') scope) scope $ Map.assocs imported'
  Right (scope', map (\(name, type') -> (name, (type', PhpValue))) $ Map.assocs imported')

analyse :: Map.Map String Scope.Scope -> String -> Parser.File -> Fallible (Scope.Scope, [(String, (Lang.Type, Value))])
analyse files path (Parser.File declarations) = do
  globalScope <- collectGlobals path declarations
  mapCatFoldlM (analyseDeclaration files) globalScope declarations
