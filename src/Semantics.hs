module Semantics (analyse, Type (..), Context, Value (..)) where

import Data.Map (Map)
import Functions (imap)
import qualified Data.Map as Map
import qualified Maps
import qualified Parser

data Type = Int | Unit | Pair Type Type | Projection Type Type deriving (Eq, Read, Show)
type Scope = Map String Value
type Context = [Scope]

data Primitive = Add | Subtract | Multiply | Divide | And | Or | Xor | Equal
    | NotEqual | LowerThan | GreaterThan | Power | Not | If | Abstract deriving (Eq, Read, Show)

data Value
    = Literal Type Integer
    | Function Type Value
    | Primitive Type Primitive
    | Argument Type Integer
    | Variable Type String Context
    | Call Type Value [Value]
    | InvalidValue String deriving (Eq, Read, Show)

operation :: Type -> Type
operation t = Projection (Pair t t) t

operator :: Primitive -> Value
operator = Primitive (operation Int)

tuple :: [Type] -> Type
tuple = foldr Pair Unit

typeOf :: Value -> Type
typeOf (Literal t _) = t
typeOf (Function t _) = t
typeOf (Primitive t _) = t
typeOf (Argument t _) = t
typeOf (Variable t _ _) = t
typeOf (Call t _ _) = t

builtins :: Scope
builtins = Map.fromList [("+", operator Add), ("-", operator Subtract), ("*", operator Multiply),
    ("/", operator Divide), ("&", operator And), ("|", operator Or), ("~", operator Xor), ("=", operator Equal),
    ("/=", operator NotEqual), ("<", operator LowerThan), (">", operator GreaterThan), ("^", operator Power),
    ("not", Primitive (Projection Int Int) Not)]

analyseExpressions :: Context -> [Parser.Expression] -> [Value]
analyseExpressions ctx = map $ analyseExpression ctx

analyseOperation :: Value -> [Value] -> Value
analyseOperation called arguments = case typeOf called of
    Projection from to | from == argTypes  -> Call to called arguments
    Projection from to -> InvalidValue $ "Expected argument types " ++ show from ++ " but got " ++ show argTypes
    _ -> InvalidValue $ "Value " ++ show called ++ " is not callable"
    where argTypes = tuple $ map typeOf arguments

analyseExpression :: Context -> Parser.Expression -> Value
analyseExpression ctx (Parser.Integer int) = Literal Int int
analyseExpression ctx (Parser.Rational _) = undefined
analyseExpression ctx (Parser.String _) = undefined
analyseExpression ctx (Parser.Variable name) = case Maps.lookup name ctx of
    Just value -> Variable (typeOf value) name ctx
    Nothing -> InvalidValue $ "Variable " ++ name ++ " does not exist"
analyseExpression ctx (Parser.Call fn args) = analyseOperation (analyseExpression ctx fn) arguments
    where arguments = map (analyseExpression ctx) args
analyseExpression ctx (Parser.Operation op a1 a2) = case Maps.lookup op ctx of
    Just operator -> analyseOperation operator $ analyseExpressions ctx [a1, a2]
    Nothing -> InvalidValue $ "Unknown operator " ++ op
analyseExpression ctx (Parser.Condition cond ifTrue ifFalse) = analyseOperation condition expressions
    where
        expressions = analyseExpressions ctx [cond, ifTrue, ifFalse]
        typ = typeOf $ expressions !! 1
        condition = Primitive (Projection (tuple [Int, typ, typ]) typ) If

analyseExpression ctx (Parser.Assignment name assigned result) = analyseExpression (Maps.insert name value ctx) result
    where value = analyseExpression ctx assigned

analyseDeclaration :: Scope -> Parser.Declaration -> Scope
analyseDeclaration scope (Parser.Function name [] result) = Map.insert name (InvalidValue "Function must have") scope
analyseDeclaration scope (Parser.Function name args result) = resultScope
    where
        argsScope = Map.fromList $ imap (\i arg -> (arg, Argument Int i)) args
        functionType = Projection (tuple $ map (const Int) args) Int
        tmpScope = Map.insert name (Primitive functionType Abstract) scope
        functionValue = analyseExpression [argsScope, tmpScope] result
        resultScope = Map.insert name (Function functionType functionValue) scope

analyse :: Parser.Program -> Scope
analyse (Parser.Program xs) = foldl analyseDeclaration builtins xs
