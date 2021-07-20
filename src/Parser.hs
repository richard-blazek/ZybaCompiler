module Parser (Expression (..), parse, parseValue) where

import Lexer (Token (..), Operator (..), Keyword (..))
import Data.Ratio ((%))

data Expression = Integer Integer
                | Rational Rational
                | String String
                | Variable String
                | Tuple [Expression]
                | BinaryOperation Operator Expression Expression
                | IfExpression [(Expression, [Expression])] [Expression]
                | WhileExpression Expression [Expression]
                | Function String [String] [Expression]
                | Dereference Expression deriving (Show, Eq)

parseValue :: [Token] -> (Expression, [Token])
parseValue (LiteralInteger _ n : tokens) = (Integer n, tokens)
parseValue (LiteralDecimal radix n exp : tokens) = (Rational (n % (radix ^ exp)), tokens)
parseValue (LiteralString string : tokens) = (String string, tokens)
parseValue (Identifier name : tokens) = (Variable name, tokens)
parseValue (ParenthesisOpen : tokens) = (expression, restTokens)
    where (expression, restTokens) = case parseBlock [ParenthesisClose] [] tokens of
            (expr : [], _, rest) -> (expr, rest)
            (expressions, _, rest) -> (Tuple expressions, rest)
parseValue (Keyword If : tokens) = parseIf [] tokens
parseValue (Keyword While : tokens) = parseWhile tokens
parseValue (Keyword Fun : tokens) = parseFunction tokens
parseValue (BracketOpen : tokens)
    | head tokensAfterExpression == BracketClose = (Dereference expression, tail tokensAfterExpression)
    where (expression, tokensAfterExpression) = parseExpression tokens

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
    Keyword Else -> (IfExpression (reverse allIfs) elseBlock, tokensAfterElse)
    Keyword End -> (IfExpression (reverse allIfs) [], tokensAfterBlock)
    Keyword Elif -> parseIf allIfs tokensAfterBlock
    where
        (condition, tokensAfterCondition) = parseExpression tokens
        (block, blockTerminator, tokensAfterBlock) = parseBlock [Keyword Else, Keyword Elif, Keyword End] [] tokensAfterCondition
        (elseBlock, _, tokensAfterElse) = parseBlock [Keyword End] [] tokensAfterBlock
        allIfs = (condition, block) : ifs

parseWhile :: [Token] -> (Expression, [Token])
parseWhile tokens = (WhileExpression conditionExpression blockExpressions, tokensAfterBlock)
    where
        (conditionExpression, tokensAfterCondition) = parseExpression tokens
        (blockExpressions, _, tokensAfterBlock) = parseBlock [Keyword End] [] tokensAfterCondition

parseFunction :: [Token] -> (Expression, [Token])
parseFunction (Identifier name : (ParenthesisOpen : tokens)) = (Function name args block, tokensAfterBlock)
    where
        parseArgList args (ParenthesisClose : tokens) = (reverse args, tokens)
        parseArgList args (Identifier name : tokens) = parseArgList (name : args) tokens
        (args, tokensAfterArgs) = parseArgList [] tokens
        (block, _, tokensAfterBlock) = parseBlock [Keyword End] [] tokensAfterArgs

parseFile :: [Expression] -> [Token] -> [Expression]
parseFile previous [] = reverse previous
parseFile previous tokens = parseFile (expression : previous) restTokens
    where (expression, restTokens) = parseExpression tokens

parse = parseFile []