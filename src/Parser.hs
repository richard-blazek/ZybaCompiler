module Parser (Expression (..), parse) where

import Lexer (Token (..), Operator (..), Keyword (..))
import Data.Ratio ((%))

data Expression = Integer Integer
                | Rational Rational
                | String String
                | Variable String
                | BinaryOperation Operator Expression Expression

parseValue :: [Token] -> (Expression, [Token])
parseValue (LiteralInteger _ n : tokens) = (Integer n, tokens)
parseValue (LiteralDecimal radix n exp : tokens) = (Rational (n % (radix ^ exp)), tokens)
parseValue (LiteralString string : tokens) = (String string, tokens)
parseValue (Identifier name : tokens) = (Variable name, tokens)

parseBinaryOperation :: Expression -> [Token] -> (Expression, [Token])
parseBinaryOperation current (Operator operator : tokens) = parseBinaryOperation operation newTokens
    where
        (operand, newTokens) = parseValue tokens
        operation = BinaryOperation operator current operand
parseBinaryOperation current tokens = (current, tokens)

parseExpression :: [Token] -> (Expression, [Token])
parseExpression tokens = parseBinaryOperation value rest
    where (value, rest) = parseValue tokens

parse :: [Token] -> Expression
parse = fst . parseExpression