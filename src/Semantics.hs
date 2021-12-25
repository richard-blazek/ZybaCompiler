module Semantics (analyse, ValueData (..), Value, Statement (..)) where

import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree
import qualified Parser
import qualified Scope
import qualified Language as Lang
import Data.Foldable (foldlM)
import Functions (pair, (??), foldlMapM, tailRecM)
import Errors (Fallible, failure, assert)

data Statement
  = Expression Value
  | Initialization String Value
  | Assignment String Value
  | While Value [Statement]
  | IfChain [(Value, [Statement])] [Statement] deriving (Eq, Show)

data ValueData
  = LiteralInt Integer
  | LiteralFloat Double
  | LiteralText String
  | LiteralBool Bool
  | Lambda [(String, Lang.Type)] [Statement]
  | Name String
  | Call Value [Value]
  | Primitive Lang.Primitive [Value] deriving (Eq, Show)

type Value = (Lang.Type, ValueData)

analyseType :: (Integer, Parser.Expression) -> Fallible Lang.Type
analyseType (line, Parser.Name name) = Lang.getType line name []
analyseType (_, Parser.Call (line, Parser.Name template) args) = mapM analyseType args >>= Lang.getType line template
analyseType (line, expr) = failure line $ "Invalid type" ++ show expr

collectGlobal :: Scope.Scope -> (Integer, Parser.Declaration) -> Fallible Scope.Scope
collectGlobal scope (_, Parser.Declaration name (line, Parser.Lambda args returned body)) = do
  argTypes <- mapM (analyseType . snd) args
  returnType <- analyseType returned
  Scope.addConstant line name (Lang.Fun argTypes returnType) scope
collectGlobal scope _ = Right scope

collectGlobals :: [(Integer, Parser.Declaration)] -> Fallible Scope.Scope
collectGlobals = foldlM collectGlobal Scope.empty

analyseCall :: Integer -> Value -> [Value] -> Fallible Value
analyseCall line callee@(type', _) args = case type' of
  Lang.Fun from to | from == types -> Right (to, Call callee args)
  Lang.Fun from to -> failure line $ "Expected arguments' types " ++ show from ++ " but got " ++ show types
  _ -> failure line $ "Expected a function but got " ++ show callee ++ " which is of a type " ++ show type'
  where types = map fst args

analyseExpression :: Scope.Scope -> (Integer, Parser.Expression) -> Fallible Value
analyseExpression scope (line, Parser.LiteralInt lit) = Right (Lang.Int, LiteralInt lit)
analyseExpression scope (line, Parser.LiteralFloat lit) = Right (Lang.Float, LiteralFloat lit)
analyseExpression scope (line, Parser.LiteralText lit) = Right (Lang.Text, LiteralText lit)
analyseExpression scope (line, Parser.LiteralBool lit) = Right (Lang.Bool, LiteralBool lit)
analyseExpression scope (line, Parser.Name name) = fmap (`pair` Name name) $ Scope.getType line name scope

analyseExpression scope (line, Parser.Call callee args) = do
  args' <- mapM (analyseExpression scope) args
  callee' <- analyseExpression scope callee
  analyseCall line callee' args'

analyseExpression scope (line, Parser.Primitive name args) = do
  args' <- mapM (analyseExpression scope) args
  primitive <- Lang.getPrimitive line name
  resultType <- Lang.getResultType line primitive $ map fst args'
  Right $ (resultType, Primitive primitive args')

analyseExpression scope (line, Parser.Field obj name) = undefined

analyseExpression scope (line, Parser.Lambda args returnType block) = do
  argTypes <- mapM (analyseType . snd) args
  let args' = zip (map fst args) argTypes
  returnType' <- analyseType returnType
  innerScope <- foldlM (\scope (name, type') -> Scope.addConstant line name type' scope) scope args'
  block' <- analyseBlock innerScope block
  case reverse block' of
    Expression (type', _) : _ | returnType' `elem` [type', Lang.Void] -> Right (Lang.Fun argTypes returnType', Lambda args' block')
    _ -> failure line $ "Function must return a value of type " ++ show returnType'

analyseStatement :: Scope.Scope -> (Integer, Parser.Statement) -> Fallible (Statement, Scope.Scope)
analyseStatement scope (line, Parser.Expression expr) = do
  value <- analyseExpression scope expr
  Right (Expression value, scope)

analyseStatement scope (line, Parser.Assignment name expr) = do
  value@(type', _) <- analyseExpression scope expr
  (newScope, added) <- Scope.addVariable line name type' scope
  Right $ if added
    then (Initialization name value, newScope)
    else (Assignment name value, scope)

analyseStatement scope (line, Parser.IfChain ifs else') = do
  ifChain <- mapM (\(cond, block) -> analyseExpression scope cond >>= (\cond' -> fmap (pair cond') $ analyseBlock scope block)) ifs
  elseBlock <- analyseBlock scope else'
  assert (all ((== Lang.Int) . fst . fst) ifChain) line $ "Condition must have an Int type"
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

analyseDeclaration :: Scope.Scope -> (Integer, Parser.Declaration) -> Fallible (Scope.Scope, (String, Value))
analyseDeclaration scope (_, Parser.Declaration name expression@(_, Parser.Lambda _ _ _)) = fmap (pair scope . pair name) $ analyseExpression scope expression
analyseDeclaration scope (line, Parser.Declaration name expression) = do
  expression@(type', _) <- analyseExpression scope expression
  newScope <- Scope.addConstant line name type' scope
  Right (newScope, (name, expression))

analyse :: [(Integer, Parser.Declaration)] -> Fallible ([(String, Value)])
analyse declarations = do
  globalScope <- collectGlobals declarations
  fmap (reverse . snd) $ foldlMapM analyseDeclaration globalScope declarations
