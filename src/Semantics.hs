module Semantics (analyse, Type (..), Primitive (..), ValueData (..), Value, StatementData (..), Statement) where

import Functions (pair, fill, (??), tryInsert, foldlMapM)
import Errors (Fallible, failure, failures)
import Data.Foldable (foldlM)
import qualified Data.Map.Strict as Map
import qualified Parser

data Type = Int | Unit | Projection [Type] Type deriving (Eq, Read, Show)
data Primitive = Add | Subtract | Multiply | Divide | Modulo | And | Or | Xor | Equal
  | NotEqual | LowerThan | GreaterThan | Power | Not deriving (Eq, Read, Show)

data StatementData
  = Evaluation Value
  | Initialization String Value
  | Assignment String Value
  | While Value Statement
  | IfChain [(Value, Statement)] Statement
  | Block [Statement] deriving (Eq, Read, Show)

data ValueData
  = Literal Integer
  | Function [(String, Type)] Statement
  | Primitive Primitive
  | Variable String
  | Operation Value [Value] deriving (Eq, Read, Show)

type Described t = (Integer, Type, t)
type Statement = Described StatementData
type Value = Described ValueData
type Declaration = Described Bool

typeOf :: (Integer, Type, a) -> Type
typeOf (_, t, _) = t

operatorS = (0, Projection [Int, Int] Int, False)
operatorV primitive = (0, Projection [Int, Int] Int, Primitive primitive)

builtinScope :: Map.Map String Declaration
builtinScope = Map.fromList [("+", operatorS), ("-", operatorS), ("*", operatorS), ("/", operatorS), ("\\", operatorS),
  ("&", operatorS), ("|", operatorS), ("^", operatorS), ("=", operatorS), ("!=", operatorS), ("<", operatorS),
  (">", operatorS), ("**", operatorS), ("not", (0, Projection [Int] Int, False))]

builtins :: Map.Map String Value
builtins = Map.fromList [("+", operatorV Add), ("-", operatorV Subtract), ("*", operatorV Multiply),
  ("/", operatorV Divide), ("%", operatorV Modulo), ("&", operatorV And), ("|", operatorV Or), ("^", operatorV Xor),
  ("=", operatorV Equal), ("!=", operatorV NotEqual), ("<", operatorV LowerThan),
  (">", operatorV GreaterThan), ("**", operatorV Power), ("not", (0, Projection [Int] Int, Primitive Not))]

analyseType :: Parser.Lined Parser.Expression -> Fallible Type
analyseType (_, Parser.Name "Unit") = Right Unit
analyseType (_, Parser.Name "Int") = Right Int
analyseType (_, Parser.Operation (_, Parser.Name "Fun") args) = do
  argTypes <- mapM analyseType args
  Right $ Projection (init argTypes) (last argTypes)
analyseType (line, expr) = failure line $ "Unknown type: " ++ show expr

processGlobalFunction :: Map.Map String Declaration -> Parser.Lined Parser.Declaration -> Fallible (Map.Map String Declaration)
processGlobalFunction scope (_, Parser.Declaration name (line, Parser.Function args returned body)) = do
  argTypes <- mapM (analyseType . snd) args
  returnType <- analyseType returned
  tryInsert (failure line $ "Redefinition of " ++ name) name (line, Projection argTypes returnType, False) scope
processGlobalFunction scope _ = Right scope

collectGlobalFunctions :: [Parser.Lined Parser.Declaration] -> Fallible (Map.Map String Declaration)
collectGlobalFunctions = foldlM processGlobalFunction builtinScope

analyseOperation :: Value -> [Value] -> Fallible Value
analyseOperation callee@(line, type', _) args = case type' of
  Projection from to | from == types -> Right (line, to, Operation callee args)
  Projection from to -> failure line $ "Expected arguments' types " ++ show from ++ " but got " ++ show types
  _ -> failure line $ "Expected a function but got " ++ show callee ++ " which is of a type " ++ show type'
  where types = map typeOf args

analyseExpression :: Map.Map String Declaration -> Parser.Lined Parser.Expression -> Fallible Value
analyseExpression scope (line, Parser.Integer int) = Right (line, Int, Literal int)
analyseExpression scope (line, Parser.Rational _) = undefined
analyseExpression scope (line, Parser.String _) = undefined
analyseExpression scope (line, Parser.Name name) = case Map.lookup name scope of
  Just (_, type', _) -> Right (line, type', Variable name)
  Nothing -> failure line $ "Unknown variable: " ++ name

analyseExpression scope (line, Parser.Operation fun params) = do
  args <- mapM (analyseExpression scope) params
  callee <- analyseExpression scope fun
  analyseOperation callee args

analyseExpression scope (line, Parser.Function args returned block) = do
  argTypes <- mapM (analyseType . snd) args
  argList <- return $ zip (map fst args) argTypes
  returnType <- analyseType returned
  innerScope <- foldlM (\scope (name, type') -> tryInsert (failure line "Duplicate arguments in a function definition") name (line, type', False) scope) scope argList
  code@(_, type', _) <- analyseBlock [] innerScope block
  if type' == returnType
    then Right (line, Projection argTypes returnType, Function (zip (map fst args) argTypes) code)
    else failure line $ "The function should return " ++ show returnType ++ " but returns " ++ show type'

analyseStatement :: Map.Map String Declaration -> Parser.Lined Parser.Statement -> Fallible (Statement, Map.Map String Declaration)
analyseStatement scope (line, Parser.Evaluation expr) = do
  value@(_, type', _) <- analyseExpression scope expr
  Right ((line, type', Evaluation value), scope)

analyseStatement scope (line, Parser.Assignment name expr) = do
  value@(_, type', _) <- analyseExpression scope expr
  case Map.lookup name scope of
    Nothing -> Right ((line, Unit, Initialization name value), Map.insert name (line, type', True) scope)
    Just (_, previousType, True) | type' == previousType -> Right ((line, Unit, Assignment name value), scope)
    Just (_, previousType, True) -> failure line $ "Assigning a value of a type " ++ show type' ++ " to the variable " ++ name ++ " which has a type " ++ show previousType
    Just (_, _, False) -> failure line $ "Attempting to assign to a constant: " ++ show name

analyseStatement scope (line, Parser.IfChain ifs else') = do
  ifChain <- mapM (\(cond, block) -> analyseExpression scope cond >>= (\cond' -> fmap (pair cond') $ analyseBlock [] scope block)) ifs
  elseBlock@(_, type', _) <- analyseBlock [] scope else'
  if all ((== Int) . typeOf . fst) ifChain
    then Right ((line, Unit, IfChain ifChain elseBlock), scope)
    else failure line $ "Condition must have an Int type"

analyseStatement scope (line, Parser.While condition body) = do
  cond <- analyseExpression scope condition
  body <- analyseBlock [] scope body
  Right ((line, Unit, While cond body), scope)

analyseBlock :: [Statement] -> Map.Map String Declaration -> [Parser.Lined Parser.Statement] -> Fallible Statement
analyseBlock [] _ [] = Right (-1, Unit, Block [])
analyseBlock result@((line, type', _) : _) scope [] = Right (line, type', Block $ reverse result)
analyseBlock result scope (statement : statements) = do
  (analysed, newScope) <- analyseStatement scope statement
  analyseBlock (analysed : result) newScope statements

analyseDeclaration :: Map.Map String Declaration -> Parser.Lined Parser.Declaration -> Fallible (Map.Map String Declaration, (String, Value))
analyseDeclaration scope (_, Parser.Declaration name expression@(_, Parser.Function _ _ _)) = fmap (pair scope . pair name) $ analyseExpression scope expression
analyseDeclaration scope (line, Parser.Declaration name expression) = do
  expression@(_, type', _) <- analyseExpression scope expression
  newScope <- tryInsert (failure line $ "Attempting to redefine " ++ name) name (line, type', False) scope
  Right (newScope, (name, expression))

analyse :: [Parser.Lined Parser.Declaration] -> Fallible ([(String, Value)])
analyse declarations = do
  globalScope <- collectGlobalFunctions declarations
  fmap snd $ foldlMapM analyseDeclaration globalScope declarations
