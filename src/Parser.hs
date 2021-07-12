module Parser (Expression (..), parse) where

import Lexer (Token (..), Operator (..), Keyword (..))
import Data.Ratio ((%))

data Expression = Integer Integer
                | Rational Rational
                | String String
                | Variable String
                | Tuple [Expression]
                | BinaryOperation Operator Expression Expression
                | IfStatement [(Expression, [Expression])] (Maybe [Expression])
                | WhileLoop (Expression, [Expression]) deriving (Show, Eq)

parseValue :: [Token] -> (Expression, [Token])
parseValue (LiteralInteger _ n : tokens) = (Integer n, tokens)
parseValue (LiteralDecimal radix n exp : tokens) = (Rational (n % (radix ^ exp)), tokens)
parseValue (LiteralString string : tokens) = (String string, tokens)
parseValue (Identifier name : tokens) = (Variable name, tokens)
parseValue (ParenthesisOpen : tokens)
    | isOne = (head expressions, restTokens)
    | otherwise = (Tuple expressions, restTokens)
    where
        (expressions, _, restTokens) = parseBlock [ParenthesisClose] [] tokens
        isOne = tail expressions == []
parseValue (Keyword If : tokens) = parseIf [] tokens
parseValue (Keyword While : tokens) = parseWhile tokens

parseOperationWith :: ([Token] -> (Expression, [Token])) -> [Operator] -> Expression -> [Token] -> (Expression, [Token])
parseOperationWith parseFn operators first tokens
    | isOperator = parseOperationWith parseFn operators (BinaryOperation operator first second) restTokens
    | otherwise = (first, tokens)
    where
        (second, restTokens) = parseFn (tail tokens)
        (isOperator, operator) = case tokens of
            Operator op : _ -> (op `elem` operators, op)
            _ -> (False, undefined)

parseExpressionWith :: ([Token] -> (Expression, [Token])) -> [Operator] -> [Token] -> (Expression, [Token])
parseExpressionWith parseFn operators tokens = parseOperationWith parseFn operators first restTokens
    where (first, restTokens) = parseFn tokens

parseApplication = parseExpressionWith parseValue [Apply]
parseExpression = parseExpressionWith parseApplication [Plus, Minus, Multiply, Divide, IntDivide, Modulo, And, Or, Xor, RaiseToThePowerOf,
    Assign, Equal, NotEqual, GreaterThan, LowerThan, GreaterThanOrEqualTo, LowerThanOrEqualTo, Apply]

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