module Parser (Expression (..), Program (..), Declaration (..), parse) where

import Lexer (Token (..))
import Data.Ratio ((%))
import Functions (pair, apply)

data Program = Program [Declaration] deriving (Show, Eq)
data Declaration
    = Function String [String] Expression
    | InvalidDeclaration String deriving (Show, Eq)

data Expression = Integer Integer
    | Rational Rational
    | String String
    | Variable String
    | Call Expression [Expression]
    | Operation String Expression Expression
    | IfExpression Expression Expression Expression
    | InvalidExpression String deriving (Show, Eq)

parseMany :: [a] -> ([Token] -> (a, [Token])) -> Token -> [Token] -> ([a], [Token])
parseMany result parser end (token : tokens)
    | token == end = (reverse result, tokens)
    | otherwise = parseMany (parsed : result) parser end restTokens
    where (parsed, restTokens) = parser $ token : tokens

expect :: [Token] -> ([Token] -> (t, [Token])) -> (String -> t) -> [Token] -> (t, [Token])
expect [] success err tokens = success tokens
expect (expected : following) success err [] = (err $ "Expected " ++ show expected ++ " but reached the EOF", [])
expect (expected : following) success err (token : tokens)
    | token == expected = expect following success err tokens
    | otherwise = (err $ "Expected " ++ show expected ++ " but got " ++ show token, tokens)

parseValue :: [Token] -> (Expression, [Token])
parseValue (LiteralInteger _ i : tokens) = (Integer i, tokens)
parseValue (LiteralDecimal _ n exp : tokens) = (Rational $ n % (10 ^ exp), tokens)
parseValue (LiteralString s : tokens) = (String s, tokens)
parseValue (Word "if" : tokens) = parseIf tokens
parseValue (Word i : tokens) = (Variable i, tokens)
parseValue (Separator '(' : tokens) = expect [Separator ')'] (pair expression) InvalidExpression restTokens
    where (expression, restTokens) = parseExpression tokens
parseValue (token : tokens) = (InvalidExpression $ "Expected a value but got " ++ show token, tokens)
parseValue [] = (InvalidExpression "Expected a value but reached the end-of-file", [])

parseBrackets :: Expression -> [Token] -> (Expression, [Token])
parseBrackets func (Separator '[' : tokens) = parseBrackets (Call func args) restTokens
    where (args, restTokens) = parseMany [] parseExpression (Separator ']') tokens
parseBrackets func tokens = (func, tokens)

parseCall :: [Token] -> (Expression, [Token])
parseCall tokens = apply (parseValue tokens) parseBrackets

parseOperation :: Expression -> [Token] -> (Expression, [Token])
parseOperation first (Operator op : tokens) = parseOperation (Operation op first second) restTokens
    where (second, restTokens) = parseCall tokens
parseOperation first tokens = (first, tokens)

parseExpression :: [Token] -> (Expression, [Token])
parseExpression tokens = apply (parseCall tokens) parseOperation

parseIf :: [Token] -> (Expression, [Token])
parseIf tokens = (IfExpression condition ifTrue ifFalse, tokensAfterElse)
    where
        (condition, tokensAfterCondition) = parseExpression tokens
        (ifTrue, tokensAfterThen) = parseExpression tokensAfterCondition
        (ifFalse, tokensAfterElse) = parseExpression tokensAfterThen

parseFunction :: String -> [Token] -> (Declaration, [Token])
parseFunction name tokens = (Function name args expression, restTokens)
    where
        parseWord (Word n : tokens) = (n, tokens)
        (args, tokensAfterArgs) = parseMany [] parseWord (Separator ']') tokens
        (expression, restTokens) = parseExpression tokensAfterArgs

parseDeclaration :: [Token] -> (Declaration, [Token])
parseDeclaration (token : tokens) = case token of
    Word name -> expect [Word "is", Word "fun", Separator '['] (parseFunction name) InvalidDeclaration tokens
    _ -> (InvalidDeclaration $ "Expected a name but got " ++ show token, tokens)

parseProgram :: [Declaration] -> [Token] -> Program
parseProgram result [] = Program $ reverse result
parseProgram result tokens = parseProgram (decl : result) restTokens
    where (decl, restTokens) = parseDeclaration tokens

parse = parseProgram []