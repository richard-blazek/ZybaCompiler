module Semantics (analyse, Type (..), Primitive (..), ValueData (..), Value, Scope) where

import Functions (pair, fill, (??), join, unionMapWith)
import Errors (Fallible, failure)
import qualified Data.Map.Strict as Map
import qualified Parser

data Type = Int | Unit | Pair Type Type | Projection Type Type deriving (Eq, Read, Show)
data Primitive = Add | Subtract | Multiply | Divide | Modulo | And | Or | Xor | Equal
    | NotEqual | LowerThan | GreaterThan | Power | Not | If deriving (Eq, Read, Show)

data ValueData
    = Literal Integer
    | Function [String] Value
    | Primitive Primitive
    | Variable String
    | Operation Value [Value] deriving (Eq, Read, Show)

type Value = (Integer, Type, ValueData)
type Scope = Map.Map String Value

tuple :: [Type] -> Type
tuple = foldr Pair Unit

typeOf :: Value -> Type
typeOf (_, t, _) = t

operator primitive = (0, Projection (tuple [Int, Int]) Int, Primitive primitive)

builtins :: Scope
builtins = Map.fromList [("+", operator Add), ("-", operator Subtract), ("*", operator Multiply),
    ("/", operator Divide), ("\\", operator Modulo), ("&", operator And), ("|", operator Or), ("~", operator Xor),
    ("=", operator Equal), ("/=", operator NotEqual), ("<", operator LowerThan),
    (">", operator GreaterThan), ("^", operator Power), ("not", (0, Projection (tuple [Int]) Int, Primitive Not))]

processDecarations :: [Parser.Declaration] -> Fallible Scope
processDecarations declarations = if null duplicates then Right scope else Left $ unlines $ map conflict duplicates
    where
        process (Parser.Function line name args result) = (name, (line, Projection (tuple $ fill Int args) Int, Variable name))
        (scope, duplicates) = unionMapWith builtins $ map process declarations
        conflict (name, (oldLine, _, _), (newLine, _, _)) = "Line " ++ show newLine ++ ": Attempting to redefine " ++ name ++ " already defined at " ++ show oldLine

analyseOperation :: Value -> [Value] -> Fallible Value
analyseOperation callee@(line, theType, _) args = case theType of
    Projection from to | from == types -> Right (line, to, Operation callee args)
    Projection from to -> failure line $ "Expected arguments' types " ++ show from ++ " but got " ++ show types
    _ -> failure line $ "Expected a function but got " ++ show callee ++ " which is of a type " ++ show theType
    where types = tuple $ map typeOf args

analyseExpression :: Scope -> Parser.Expression -> Fallible Value
analyseExpression scope (Parser.Integer line int) = Right (line, Int, Literal int)
analyseExpression scope (Parser.Rational _ _) = undefined
analyseExpression scope (Parser.String _ _) = undefined
analyseExpression scope (Parser.Name line name) = fmap Right (Map.lookup name scope) ?? failure line ("There is no such thing as " ++ name)

analyseExpression scope (Parser.Operation fun params) = do
    args <- mapM (analyseExpression scope) params
    callee <- analyseExpression scope fun
    analyseOperation callee args

analyseExpression scope (Parser.Condition condition ifTrue ifFalse) = do
    cond@(line, _, _) <- analyseExpression scope condition
    ift@(_, theType, _) <- analyseExpression scope ifTrue
    iff <- analyseExpression scope ifFalse
    analyseOperation (line, Projection (tuple [Int, theType, theType]) theType, Primitive If) [cond, ift, iff]

analyseExpression scope (Parser.Assignment var assignee result) = do
    assigneeValue <- analyseExpression scope assignee
    analyseExpression (Map.insert var assigneeValue scope) result

analyseDeclaration :: Scope -> Parser.Declaration -> Fallible (String, Value)
analyseDeclaration scope (Parser.Function line name args expression) = do
    expressionValue <- analyseExpression innerScope expression
    Right (name, (line, Projection (tuple $ fill Int args) Int, Function args expressionValue))
    where innerScope = Map.union scope $ Map.fromList $ map (\x -> (x, (line, Int, Variable x))) args

analyse :: [Parser.Declaration] -> Fallible Scope
analyse declarations = do
    globalScope <- processDecarations declarations
    declarationPairs <- mapM (analyseDeclaration globalScope) declarations
    Right $ Map.fromList declarationPairs
