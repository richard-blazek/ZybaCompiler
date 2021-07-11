module Parser (Expression (..), parse) where

import Lexer (Token (..), Operator (..), Keyword (..))
import Data.Ratio ((%))

data Expression = Integer Integer
                | Rational Rational
                | String String
                | Variable String
                | BinaryOperation Operator Expression Expression
                | Conditional [(Expression, [Expression])] (Maybe [Expression]) deriving (Show, Eq)

parseValue :: [Token] -> (Expression, [Token])
parseValue (LiteralInteger _ n : tokens) = (Integer n, tokens)
parseValue (LiteralDecimal radix n exp : tokens) = (Rational (n % (radix ^ exp)), tokens)
parseValue (LiteralString string : tokens) = (String string, tokens)
parseValue (Identifier name : tokens) = (Variable name, tokens)
parseValue (ParenthesisOpen : tokens) = case parseExpression tokens of 
    (expression, ParenthesisClose : restTokens) -> (expression, restTokens)

parseBinaryOperation :: Expression -> [Token] -> (Expression, [Token])
parseBinaryOperation current (Operator operator : tokens) = parseBinaryOperation operation restTokens
    where
        (operand, restTokens) = parseValue tokens
        operation = BinaryOperation operator current operand
parseBinaryOperation current tokens = (current, tokens)

parseBlock :: [Expression] -> [Token] -> ([Expression], [Token])
parseBlock expressions tokens
    | head tokens `elem` [Keyword Else, Keyword Elif, Keyword End] = (reverse expressions, tokens)
    | otherwise = parseBlock (firstExpression : expressions) restTokens
    where (firstExpression, restTokens) = parseExpression tokens

parseConditional :: [(Expression, [Expression])] -> [Token] -> (Expression, [Token])
parseConditional ifs tokens = case head tokensAfterBlock of
    Keyword Else -> (Conditional (reverse allIfs) (Just elseBlock), tokensAfterElse)
    Keyword End -> (Conditional (reverse allIfs) Nothing, tokensAfterBlock)
    Keyword Elif -> parseConditional allIfs tokensAfterBlock
    where
        (conditionExpression, tokensAfterCondition) = parseExpression tokens
        (blockExpressions, tokensAfterBlock) = parseBlock [] tokensAfterCondition
        (elseBlock, tokensAfterElse) = parseBlock [] (tail tokensAfterBlock)
        allIfs = (conditionExpression, blockExpressions) : ifs

parseExpression :: [Token] -> (Expression, [Token])
parseExpression (Keyword If : tokens) = parseConditional [] tokens
parseExpression tokens = parseBinaryOperation value restTokens
    where (value, restTokens) = parseValue tokens

parse :: [Token] -> Expression
parse = fst . parseExpression