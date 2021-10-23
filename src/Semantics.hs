{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Semantics (analyse, Type (..), Program, Primitive (..), Value (..)) where

import Data.Map (Map)
import Functions (pair)
import Errors (Fallible, invalid)
import qualified Data.Map.Strict as Map
import qualified Parser

data Type = Int | Unit | Pair Type Type | Projection Type Type | InvalidType String deriving (Eq, Read, Show)
type Scope = Map String Type
type Program = Map String Value

data Primitive = Add | Subtract | Multiply | Divide | And | Or | Xor | Equal
    | NotEqual | LowerThan | GreaterThan | Power | Not | If | Abstract deriving (Eq, Read, Show)

data Value
    = Literal Type Integer
    | Function Type [String] Value
    | Primitive Type Primitive
    | Binding Type String Value Value
    | Name Type String
    | Call Type Value [Value]
    | InvalidValue String deriving (Eq, Read, Show)

instance Fallible Type where
    invalid = InvalidType

instance Fallible Value where
    invalid = InvalidValue

typeOf :: Value -> Type
typeOf (Literal t _) = t
typeOf (Function t _ _) = t
typeOf (Primitive t _) = t
typeOf (Name t _) = t
typeOf (Call t _ _) = t
typeOf (Binding t _ _ _) = t
typeOf (InvalidValue _) = Unit

tuple :: [Type] -> Type
tuple = foldr Pair Unit

operator = Primitive $ Projection (tuple [Int, Int]) Int

builtins :: Program
builtins = Map.fromList [("+", operator Add), ("-", operator Subtract), ("*", operator Multiply),
    ("/", operator Divide), ("&", operator And), ("|", operator Or), ("~", operator Xor),
    ("=", operator Equal), ("/=", operator NotEqual), ("<", operator LowerThan),
    (">", operator GreaterThan), ("^", operator Power), ("not", Primitive (Projection (tuple [Int]) Int) Not)]

processDeclaration :: Parser.Declaration -> (String, Type)
processDeclaration (Parser.Function name args result) = (name, Projection (tuple $ map (const Int) args) Int)

conflict :: (Fallible f) => String -> b -> c -> f
conflict name _ _ = invalid $ "Attempting to redefine " ++ name

createGlobalScope :: Parser.Program -> Scope
createGlobalScope (Parser.Program declarations) = Map.unionWithKey conflict declared $ Map.map typeOf builtins
    where declared = Map.fromListWithKey conflict $ map processDeclaration declarations

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

analyseExpression scope (Parser.Name name) = case Map.lookup name scope of
    Just typ -> Name typ name
    Nothing -> InvalidValue $ "Name " ++ name ++ " is unknown"

analyseExpression scope (Parser.Operation fn params) = analyseCall (analyseExpression scope fn) args
    where args = map (analyseExpression scope) params

analyseExpression scope (Parser.Condition condition ifTrue ifFalse) = analyseCall fn [cond, ift, iff]
    where
        [cond, ift, iff] = map (analyseExpression scope) [condition, ifTrue, ifFalse]
        typ = typeOf ift
        fn = Primitive (Projection (tuple [Int, typ, typ]) typ) If

analyseExpression scope (Parser.Assignment var assignee result) = Binding (typeOf resultValue) var bound resultValue
    where
        bound = analyseExpression scope assignee
        resultValue = analyseExpression (Map.insert var (typeOf bound) scope) result

analyseDeclaration :: Scope -> Parser.Declaration -> (String, Value)
analyseDeclaration scope (Parser.Function name args result) = (name, function)
    where
        functionType = Projection (tuple $ map (const Int) args) Int
        innerScope = Map.union (Map.fromListWithKey conflict $ map (`pair` Int) args) scope
        function = Function functionType args $ analyseExpression innerScope result

analyse :: Parser.Program -> Program
analyse (Parser.Program declarations) = Map.unionWithKey conflict builtins definitions
    where
        scope = createGlobalScope $ Parser.Program declarations
        definitions = Map.fromListWithKey conflict $ map (analyseDeclaration scope) declarations

