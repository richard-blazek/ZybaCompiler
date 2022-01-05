module Semantics (analyse, Expression (..), Statement (..)) where

import qualified Data.Map.Strict as Map
import qualified Parser
import qualified Scope
import qualified Language as Lang
import Data.Foldable (foldlM)
import Functions (pair, (??), foldlMapM, tailRecM, fmap2)
import Fallible (Fallible (..), failure, assert)

data Statement
  = Expression (Lang.Type, Expression)
  | Initialization String (Lang.Type, Expression)
  | Assignment String (Lang.Type, Expression)
  | While (Lang.Type, Expression) [Statement]
  | IfChain [((Lang.Type, Expression), [Statement])] [Statement] deriving (Eq, Show)

data Expression
  = LiteralInt Integer
  | LiteralFloat Double
  | LiteralText String
  | LiteralBool Bool
  | LiteralRecord (Map.Map String (Lang.Type, Expression))
  | Lambda [(String, Lang.Type)] [Statement]
  | Name String
  | Call (Lang.Type, Expression) [(Lang.Type, Expression)]
  | Primitive Lang.Primitive [(Lang.Type, Expression)]
  | Access (Lang.Type, Expression) String deriving (Eq, Show)

analyseType :: Scope.Scope -> (Integer, Parser.Expression) -> Fallible Lang.Type
analyseType scope expr = fmap fst $ analyseExpression scope expr

collectGlobal :: Scope.Scope -> (Integer, Parser.Declaration) -> Fallible Scope.Scope
collectGlobal scope (_, Parser.Declaration name (line, Parser.Lambda args returned body)) = do
  argTypes <- mapM (analyseType scope . snd) args
  returnType <- analyseType scope returned
  Scope.addConstant line name (Lang.Function argTypes returnType) scope
collectGlobal scope _ = Ok scope

collectGlobals :: [(Integer, Parser.Declaration)] -> Fallible Scope.Scope
collectGlobals = foldlM collectGlobal Scope.empty

analyseCall :: Integer -> (Lang.Type, Expression) -> [(Lang.Type, Expression)] -> Fallible (Lang.Type, Expression)
analyseCall line callee@(type', _) args = case type' of
  Lang.Function from to | from == types -> Ok (to, Call callee args)
  Lang.Function from to -> failure line $ "Expected arguments' types " ++ show from ++ " but got " ++ show types
  _ -> failure line $ "Expected a function but got " ++ show callee ++ " which is of a type " ++ show type'
  where types = map fst args

analyseAccess :: Integer -> Scope.Scope -> String -> [(Lang.Type, Expression)] -> Bool -> Fallible (Lang.Type, Expression)
analyseAccess line scope name args@(obj : rest) called = case Lang.fieldAccess line name (fst obj) of
  Ok type' | called -> analyseCall line (type', Access obj name) rest
  Ok type' -> Ok (type', Access obj name)
  Error _ -> fmap2 id (flip Primitive args) $ Lang.primitiveCall line name $ map fst args

analyseExpression :: Scope.Scope -> (Integer, Parser.Expression) -> Fallible (Lang.Type, Expression)
analyseExpression scope (line, Parser.LiteralInt lit) = Ok (Lang.Int, LiteralInt lit)
analyseExpression scope (line, Parser.LiteralFloat lit) = Ok (Lang.Float, LiteralFloat lit)
analyseExpression scope (line, Parser.LiteralText lit) = Ok (Lang.Text, LiteralText lit)
analyseExpression scope (line, Parser.LiteralBool lit) = Ok (Lang.Bool, LiteralBool lit)
analyseExpression scope (line, Parser.Name name) = fmap (`pair` Name name) $ Scope.getType line name scope

analyseExpression scope (line, Parser.Call callee args) = do
  args' <- mapM (analyseExpression scope) args
  callee' <- analyseExpression scope callee
  analyseCall line callee' args'

analyseExpression scope (line, Parser.Access obj name args) = do
  args' <- mapM (analyseExpression scope) (obj : (args ?? []))
  analyseAccess line scope name args' (args /= Nothing)

analyseExpression scope (line, Parser.Lambda args returnType block) = do
  argTypes <- mapM (analyseType scope . snd) args
  let args' = zip (map fst args) argTypes
  returnType' <- analyseType scope returnType
  innerScope <- foldlM (\scope (name, type') -> fmap fst $ Scope.addVariable False line name type' scope) scope args'
  block' <- analyseBlock innerScope block
  case reverse block' of
    Expression (type', _) : _ | returnType' `elem` [type', Lang.Void] -> Ok (Lang.Function argTypes returnType', Lambda args' block')
    _ -> failure line $ "Function must return a value of type " ++ show returnType'

analyseExpression scope (line, Parser.LiteralRecord fields) = do
  fields' <- mapM (analyseExpression scope) fields
  Ok (Lang.Record $ Map.map fst fields', LiteralRecord $ fields')

analyseStatement :: Scope.Scope -> (Integer, Parser.Statement) -> Fallible (Statement, Scope.Scope)
analyseStatement scope (line, Parser.Expression expr) = do
  value <- analyseExpression scope expr
  Ok (Expression value, scope)

analyseStatement scope (line, Parser.Assignment name expr) = do
  value@(type', _) <- analyseExpression scope expr
  (newScope, added) <- Scope.addVariable True line name type' scope
  Ok . (`pair` newScope) $ if added
    then Initialization name value
    else Assignment name value

analyseStatement scope (line, Parser.IfChain ifs else') = do
  ifChain <- mapM (\(cond, block) -> analyseExpression scope cond >>= (\cond' -> fmap (pair cond') $ analyseBlock scope block)) ifs
  elseBlock <- analyseBlock scope else'
  assert (all ((== Lang.Bool) . fst . fst) ifChain) line $ "Condition must have a bool type"
  Ok (IfChain ifChain elseBlock, scope)

analyseStatement scope (line, Parser.While condition body) = do
  cond <- analyseExpression scope condition
  body <- analyseBlock scope body
  Ok (While cond body, scope)

analyseBlock :: Scope.Scope -> [(Integer, Parser.Statement)] -> Fallible [Statement]
analyseBlock scope statements = tailRecM if' then' else' ([], scope, statements)
  where if' (_, _, statements) = Ok $ null statements
        then' (result, _, _) = Ok $ reverse result
        else' (result, scope, statement : statements) = fmap (\(analysed, sc) -> (analysed : result, sc, statements)) $ analyseStatement scope statement

analyseDeclaration :: Scope.Scope -> (Integer, Parser.Declaration) -> Fallible (Scope.Scope, (String, (Lang.Type, Expression)))
analyseDeclaration scope (_, Parser.Declaration name expression@(_, Parser.Lambda _ _ _)) = fmap (pair scope . pair name) $ analyseExpression scope expression
analyseDeclaration scope (line, Parser.Declaration name expression) = do
  expression@(type', _) <- analyseExpression scope expression
  newScope <- Scope.addConstant line name type' scope
  Ok (newScope, (name, expression))

analyse :: Parser.File -> Fallible [(String, (Lang.Type, Expression))]
analyse (Parser.File declarations) = do
  globalScope <- collectGlobals declarations
  fmap (reverse . snd) $ foldlMapM analyseDeclaration globalScope declarations
