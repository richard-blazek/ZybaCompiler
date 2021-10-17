module Semantics (analyse, Type (..), Context, Value (..)) where

import Map (Map)
import Data.List.Index (imap)
import qualified Map
import qualified Maps
import qualified Parser

data Type = Int | Pair Type Type | Function Type Type deriving (Eq, Read, Show)
type Scope = Map String Value
type Context = [Scope]

data Primitive = Add | Subtract | Multiply | If | Abstract deriving (Eq, Read, Show)

data Value
    = Literal Type Integer
    | Function Type Value
    | Primitive Type Primitive
    | Argument Type Integer
    | Variable String Context
    | Operation Value [Value]
    | InvalidValue String deriving (Eq, Read, Show)

analyseExpression :: Context -> Parser.Expression -> Value
analyseExpression ctx (Integer int) = Literal Int int
analyseExpression ctx (Rational _) = undefined
analyseExpression ctx (String _) = undefined
analyseExpression ctx (Variable name) = undefined
analyseExpression ctx (Call fn args) = undefined
analyseExpression ctx (Operation op a1 a2) = undefined
analyseExpression ctx (Condition cond t e) = undefined
analyseExpression ctx (Assignment name value result) = undefined

analyseDeclaration :: Scope -> Parser.Declaration -> Scope
analyseDeclaration scope (Parser.Function name args result) = resultScope
    where
        argsScope = Map.fromList $ imap (\i arg -> (arg, Argument Int i)) args
        functionType = Function $ foldr Pair Int $ map (const Int) args
        tmpScope = Map.insert name (Primitive functionType Abstract) scope
        functionValue = analyseExpression [argsScope, tmpScope] result
        resultScope = Map.insert name (Function functionType functionValue) scope

analyse :: Parser.Program -> Scope
analyse (Parser.Program xs) = foldl analyseDeclaration Map.empty xs
