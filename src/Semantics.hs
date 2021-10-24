module Semantics (analyse, Type (..), Scope, Primitive (..), Value (..)) where

import Data.Map (Map)
import Functions (pair, fill, (??))
import Errors (Fallible, invalid)
import qualified Data.Map.Strict as Map
import qualified Parser

data Type = Int | Unit | Pair Type Type | Projection Type Type | InvalidType String deriving (Eq, Read, Show)
data Primitive = Add | Subtract | Multiply | Divide | And | Or | Xor | Equal
    | NotEqual | LowerThan | GreaterThan | Power | Not | If | Abstract deriving (Eq, Read, Show)

data Value
    = Literal Type Integer
    | Function Type [String] Value
    | Primitive Type Primitive
    | Global Type String
    | Argument Type String
    | Call Type Value [Value]
    | InvalidValue String deriving (Eq, Read, Show)

type Scope = Map String Value

instance Fallible Type where
    invalid = InvalidType

instance Fallible Value where
    invalid = InvalidValue

tuple :: [Type] -> Type
tuple = foldr Pair Unit

typeOf :: Value -> Type
typeOf (Literal t _) = t
typeOf (Function t _ _) = t
typeOf (Primitive t _) = t
typeOf (Argument t _) = t
typeOf (Global t _) = t
typeOf (Call t _ _) = t
typeOf (InvalidValue _) = Unit

operator = Primitive $ Projection (tuple [Int, Int]) Int

builtins :: Scope
builtins = Map.fromList [("+", operator Add), ("-", operator Subtract), ("*", operator Multiply),
    ("/", operator Divide), ("&", operator And), ("|", operator Or), ("~", operator Xor),
    ("=", operator Equal), ("/=", operator NotEqual), ("<", operator LowerThan),
    (">", operator GreaterThan), ("^", operator Power), ("not", Primitive (Projection (tuple [Int]) Int) Not)]

conflict :: (Fallible f) => String -> b -> c -> f
conflict name _ _ = invalid $ "Attempting to redefine " ++ name

createGlobalScope :: [Parser.Declaration] -> Scope
createGlobalScope declarations = Map.unionWithKey conflict declared builtins
    where
        process (Parser.Function name args result) = (name, Global (Projection (tuple $ fill Int args) Int) name)
        declared = Map.fromListWithKey conflict $ map process declarations

analyseCall :: Value -> [Value] -> Value
analyseCall callee args = case typeOf callee of
    Projection from to | from == types  -> Call to callee args
    Projection from to -> InvalidValue $ "Function " ++ show callee ++ " expected " ++ show from ++ " but got " ++ show types
    _ -> InvalidValue $ "Expected function but got " ++ show callee ++ " which is of a type " ++ show (typeOf callee)
    where types = tuple $ map typeOf args

analyseExpression :: Scope -> Parser.Expression -> Value
analyseExpression scope (Parser.Integer int) = Literal Int int
analyseExpression scope (Parser.Rational _) = undefined
analyseExpression scope (Parser.String _) = undefined
analyseExpression scope (Parser.Name name) = Map.lookup name scope ?? InvalidValue ("Name " ++ name ++ " is unknown")

analyseExpression scope (Parser.Operation fn params) = analyseCall (analyseExpression scope fn) args
    where args = map (analyseExpression scope) params

analyseExpression scope (Parser.Condition condition ifTrue ifFalse) = analyseCall fn [cond, ift, iff]
    where
        [cond, ift, iff] = map (analyseExpression scope) [condition, ifTrue, ifFalse]
        typ = typeOf ift
        fn = Primitive (Projection (tuple [Int, typ, typ]) typ) If

analyseExpression scope (Parser.Assignment var assignee result) = value
    where value = analyseExpression (Map.insert var (analyseExpression scope assignee) scope) result

analyseDeclaration :: Scope -> Parser.Declaration -> (String, Value)
analyseDeclaration scope (Parser.Function name args expression) = (name, function)
    where
        functionType = Projection (tuple $ fill Int args) Int
        innerScope = Map.union (Map.fromListWithKey conflict $ map (\x -> (x, Argument Int x)) args) scope
        function = Function functionType args $ analyseExpression innerScope expression

analyse :: [Parser.Declaration] -> Scope
analyse declarations = Map.unionWithKey conflict builtins $ Map.fromListWithKey conflict scopeItems
    where scopeItems = map (analyseDeclaration $ createGlobalScope declarations) declarations
