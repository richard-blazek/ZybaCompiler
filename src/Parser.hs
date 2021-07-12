module Parser (Expression (..), parse) where

import Lexer (Token (..), Operator (..), Keyword (..))
import Data.Ratio ((%))

data Expression = Integer Integer
                | Rational Rational
                | String String
                | Variable String
                | Call Expression [Expression]
                | BinaryOperation Operator Expression Expression
                | IfStatement [(Expression, [Expression])] (Maybe [Expression])
                | WhileLoop (Expression, [Expression]) deriving (Show, Eq)

parseValue :: [Token] -> (Expression, [Token])
parseValue (LiteralInteger _ n : tokens) = (Integer n, tokens)
parseValue (LiteralDecimal radix n exp : tokens) = (Rational (n % (radix ^ exp)), tokens)
parseValue (LiteralString string : tokens) = (String string, tokens)
parseValue (Identifier name : tokens) = (Variable name, tokens)
parseValue (ParenthesisOpen : tokens) = case parseExpression tokens of 
    (expression, ParenthesisClose : restTokens) -> (expression, restTokens)
parseValue (Keyword If : tokens) = parseIf [] tokens
parseValue (Keyword While : tokens) = parseWhile tokens

parseBrackets :: Expression -> [Token] -> (Expression, [Token])
parseBrackets callable (BracketOpen : tokens) = parseBrackets newCallable tokensAfterCall
    where
        (arguments, _, tokensAfterCall) = parseBlock [BracketClose] [] tokens
        newCallable = Call callable arguments
parseBrackets callable tokens = (callable, tokens)

parseCall :: [Token] -> (Expression, [Token])
parseCall tokens = parseBrackets value restTokens
    where (value, restTokens) = parseValue tokens

parseBinaryOperation :: Expression -> [Token] -> (Expression, [Token])
parseBinaryOperation current (Operator operator : tokens) = parseBinaryOperation operation restTokens
    where
        (operand, restTokens) = parseCall tokens
        operation = BinaryOperation operator current operand
parseBinaryOperation current tokens = (current, tokens)

parseExpression :: [Token] -> (Expression, [Token])
parseExpression tokens = parseBinaryOperation value restTokens
    where (value, restTokens) = parseCall tokens

parseBlock :: [Token] -> [Expression] -> [Token] -> ([Expression], Token, [Token])
parseBlock terminators expressions tokens
    | head tokens `elem` terminators = (reverse expressions, head tokens, tail tokens)
    | otherwise = parseBlock terminators (firstExpression : expressions) restTokens
    where (firstExpression, restTokens) = parseExpression tokens

parseIf :: [(Expression, [Expression])] -> [Token] -> (Expression, [Token])
parseIf ifs tokens = case blockTerminator of
    Keyword Else -> (IfStatement (reverse allIfs) (Just elseBlock), tokensAfterElse)
    Keyword End -> (IfStatement (reverse allIfs) Nothing, tokensAfterBlock)
    Keyword Elif -> parseIf allIfs tokensAfterBlock
    where
        (condition, tokensAfterCondition) = parseExpression tokens
        (block, blockTerminator, tokensAfterBlock) = parseBlock [Keyword Else, Keyword Elif, Keyword End] [] tokensAfterCondition
        (elseBlock, _, tokensAfterElse) = parseBlock [Keyword End] [] (tail tokensAfterBlock)
        allIfs = (condition, block) : ifs

parseWhile :: [Token] -> (Expression, [Token])
parseWhile tokens = (WhileLoop (conditionExpression, blockExpressions), tokensAfterBlock)
    where
        (conditionExpression, tokensAfterCondition) = parseExpression tokens
        (blockExpressions, _, tokensAfterBlock) = parseBlock [Keyword End] [] tokensAfterCondition

parse :: [Token] -> Expression
parse = fst . parseExpression