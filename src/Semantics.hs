module Semantics (analyse, Expression (..), Statement (..)) where

import qualified Data.Map.Strict as Map
import qualified Parser
import qualified Scope
import qualified Language as Lang
import Data.Foldable (foldlM)
import Functions (pair, mapCatFoldlM, tailRecM, fmap2)
import Fallible (Fallible, err, assert)

data Statement
  = Expression (Lang.Type, Expression)
  | Initialization String (Lang.Type, Expression)
  | Assignment String (Lang.Type, Expression)
  | While (Lang.Type, Expression) [Statement]
  | IfChain [((Lang.Type, Expression), [Statement])] [Statement] deriving (Eq, Show)

data Expression
  = Literal Parser.Literal
  | Record (Map.Map String (Lang.Type, Expression))
  | Lambda [(String, Lang.Type)] [Statement]
  | Name String String
  | Call (Lang.Type, Expression) [(Lang.Type, Expression)]
  | Primitive Lang.Primitive [(Lang.Type, Expression)]
  | Access (Lang.Type, Expression) String
  | PhpValue deriving (Eq, Show)

analyseType :: Scope.Scope -> (Integer, Parser.Expression) -> Fallible Lang.Type
analyseType scope expr = fmap fst $ analyseExpression scope expr

collectGlobal :: Scope.Scope -> (Integer, Parser.Declaration) -> Fallible Scope.Scope
collectGlobal scope (_, Parser.Declaration name (line, Parser.Lambda args returned body)) = do
  argTypes <- mapM (analyseType scope . snd) args
  returnType <- analyseType scope returned
  fmap fst $ Scope.add False line name (Scope.Constant $ Lang.Function argTypes returnType) scope
collectGlobal scope _ = Right scope

collectGlobals :: String -> [(Integer, Parser.Declaration)] -> Fallible Scope.Scope
collectGlobals = foldlM collectGlobal . Scope.empty

analyseCall :: Integer -> (Lang.Type, Expression) -> [(Lang.Type, Expression)] -> Fallible (Lang.Type, Expression)
analyseCall line callee@(type', _) args = case type' of
  Lang.Function from to | from == types -> Right (to, Call callee args)
  Lang.Function from to -> err line $ "Expected arguments' types " ++ show from ++ " but got " ++ show types
  _ -> err line $ "Expected a function but got " ++ show callee ++ " which is of a type " ++ show type'
  where types = map fst args

analyseAccess :: Integer -> Scope.Scope -> [String] -> (Lang.Type, Expression) -> Fallible (Lang.Type, Expression)
analyseAccess line scope [] obj = Right obj
analyseAccess line scope (name : names) obj = Lang.fieldAccess line name (fst obj) >>= analyseAccess line scope names . (`pair` Access obj name)

resolveNamespace :: [String] -> Scope.Scope -> (Integer, Parser.Expression) -> Fallible (Lang.Type, Expression)
resolveNamespace parts scope (_, Parser.Access obj name) = resolveNamespace (name : parts) scope obj
resolveNamespace parts scope (line, Parser.Name name) = do
  (type', path, remaining) <- Scope.get line (name : parts) scope
  let expr = (type', Name path name)
  if null remaining
    then Right expr
    else analyseAccess line scope remaining expr
resolveNamespace parts scope expr@(line, _) = analyseExpression scope expr >>= analyseAccess line scope parts

analyseExpression :: Scope.Scope -> (Integer, Parser.Expression) -> Fallible (Lang.Type, Expression)
analyseExpression scope (_, Parser.Literal lit@(Parser.Int _)) = Right (Lang.Int, Literal lit)
analyseExpression scope (_, Parser.Literal lit@(Parser.Real _)) = Right (Lang.Real, Literal lit)
analyseExpression scope (_, Parser.Literal lit@(Parser.Text _)) = Right (Lang.Text, Literal lit)
analyseExpression scope (_, Parser.Literal lit@(Parser.Bool _)) = Right (Lang.Bool, Literal lit)
analyseExpression scope expr@(_, Parser.Name _) = resolveNamespace [] scope expr
analyseExpression scope expr@(_, Parser.Access _ _) = resolveNamespace [] scope expr

analyseExpression scope (line, Parser.Call callee args) = do
  args' <- mapM (analyseExpression scope) args
  case callee of
    (_, Parser.Name name) -> do
      case Lang.primitiveCall line name $ map fst args' of
        Right (type', primitive) -> Right (type', Primitive primitive args')
        _ -> analyseExpression scope callee >>= flip (analyseCall line) args'
    _ -> analyseExpression scope callee >>= flip (analyseCall line) args'

analyseExpression scope (line, Parser.Lambda args returnType block) = do
  argTypes <- mapM (analyseType scope . snd) args
  let args' = zip (map fst args) argTypes
  returnType' <- analyseType scope returnType
  innerScope <- foldlM (\scope (name, type') -> fmap fst $ Scope.add False line name (Scope.Variable type') scope) scope args'
  block' <- analyseBlock innerScope block
  case reverse block' of
    Expression (type', _) : _ | returnType' `elem` [type', Lang.Void] -> Right (Lang.Function argTypes returnType', Lambda args' block')
    _ -> err line $ "Function must return a value of type " ++ show returnType'

analyseExpression scope (line, Parser.Record fields) = do
  fields' <- mapM (analyseExpression scope) fields
  Right (Lang.Record $ Map.map fst fields', Record $ fields')

analyseStatement :: Scope.Scope -> (Integer, Parser.Statement) -> Fallible (Statement, Scope.Scope)
analyseStatement scope (line, Parser.Expression expr) = do
  value <- analyseExpression scope expr
  Right (Expression value, scope)

analyseStatement scope (line, Parser.Assignment name expr) = do
  value@(type', _) <- analyseExpression scope expr
  (scope', added) <- Scope.add True line name (Scope.Variable type') scope
  Right . (`pair` scope') $ if added
    then Initialization name value
    else Assignment name value

analyseStatement scope (line, Parser.IfChain ifs else') = do
  ifChain <- mapM (\(cond, block) -> analyseExpression scope cond >>= (\cond' -> fmap (pair cond') $ analyseBlock scope block)) ifs
  elseBlock <- analyseBlock scope else'
  assert (all ((== Lang.Bool) . fst . fst) ifChain) line $ "Condition must have a bool type"
  Right (IfChain ifChain elseBlock, scope)

analyseStatement scope (line, Parser.While condition body) = do
  cond <- analyseExpression scope condition
  body <- analyseBlock scope body
  Right (While cond body, scope)

analyseBlock :: Scope.Scope -> [(Integer, Parser.Statement)] -> Fallible [Statement]
analyseBlock scope statements = tailRecM if' then' else' ([], scope, statements)
  where if' (_, _, statements) = Right $ null statements
        then' (result, _, _) = Right $ reverse result
        else' (result, scope, statement : statements) = fmap (\(analysed, sc) -> (analysed : result, sc, statements)) $ analyseStatement scope statement

analyseDeclaration :: Map.Map String Scope.Scope -> Scope.Scope -> (Integer, Parser.Declaration) -> Fallible (Scope.Scope, [(String, (Lang.Type, Expression))])
analyseDeclaration files scope (line, Parser.Import name path) = fmap ((`pair` []) . fst) $ Scope.add False line name (Scope.Namespace $ files Map.! path) scope
analyseDeclaration _ scope (_, Parser.Declaration name expression@(_, Parser.Lambda _ _ _)) = fmap (pair scope . (:[]) . pair name) $ analyseExpression scope expression
analyseDeclaration _ scope (line, Parser.Declaration name expression) = do
  expression@(type', _) <- analyseExpression scope expression
  scope' <- fmap fst $ Scope.add False line name (Scope.Constant type') scope
  Right (scope', [(name, expression)])

analyseDeclaration files scope (line, Parser.Php _ imported) = do
  imported' <- mapM (fmap fst . analyseExpression scope) imported
  scope' <- foldlM (\scope (name, type') -> fmap fst $ Scope.add False line name (Scope.Constant type') scope) scope $ Map.assocs imported'
  Right (scope', map (\(name, type') -> (name, (type', PhpValue))) $ Map.assocs imported')

analyse :: Map.Map String Scope.Scope -> String -> Parser.File -> Fallible (Scope.Scope, [(String, (Lang.Type, Expression))])
analyse files path (Parser.File declarations) = do
  globalScope <- collectGlobals path declarations
  mapCatFoldlM (analyseDeclaration files) globalScope declarations
