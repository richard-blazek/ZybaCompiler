module Parser (Expression (..), parse) where

import Lexer (Token (..), Operator (..), Keyword (..))
import Data.Ratio ((%))

data Expression = Integer Integer
    | Rational Rational
    | String String
    | Variable String
    | Call Expression [Expression]
    | BinaryOperation Operator Expression Expression
    | IfExpression [(Expression, [Expression])] [Expression]
    | WhileExpression Expression [Expression]
    | Function String [String] [Expression]
    | Error String deriving (Show, Eq)

parseValue :: [Token] -> (Expression, [Token])
parseValue (LiteralInteger _ i : tokens) = (Integer i, tokens)
parseValue (LiteralDecimal _ n exp : tokens) = (Rational $ n % (10 ^ exp), tokens)
parseValue (LiteralString s : tokens) = (String s, tokens)
parseValue (Identifier i : tokens) = (Variable i, tokens)
parseValue (Operator ParenthesisOpen _ : tokens) = case tokensAfterExpression of
    Operator ParenthesisClose _ : restTokens -> (expression, restTokens)
    _ -> (Error "Missing ')' parenthesis", tokensAfterExpression)
    where (expression, tokensAfterExpression) = parseExpression tokens
parseValue (Keyword If _ : tokens) = parseIf [] tokens
parseValue (Keyword While _ : tokens) = parseWhile tokens
parseValue (Keyword Fun _ : tokens) = parseFunction tokens
parseValue (token : tokens) = (Error $ "Unexpected token " ++ show token, tokens)
parseValue [] = (Error "No tokens", [])

parseCallArgs :: Expression -> [Token] -> (Expression, [Token])
parseCallArgs func (Operator BracketOpen _ : tokens) = parseCallArgs (Call func args) tokensAfterArgs
    where (args, _, tokensAfterArgs) = parseBlock parseExpression [Operator BracketClose ""] tokens
parseCallArgs func tokens = (func, tokens)

parseCall :: [Token] -> (Expression, [Token])
parseCall tokens = parseCallArgs func restTokens
    where (func, restTokens) = parseValue tokens

parseOperationWith :: Expression -> [Operator] -> [Token] -> (Expression, [Token])
parseOperationWith first operators tokens
    | isOperator = parseOperationWith operation operators restTokens
    | otherwise = (first, tokens)
    where
        (second, restTokens) = parseCall (tail tokens)
        (isOperator, operation) = case tokens of
            Operator op _ : _ -> (op `elem` operators, BinaryOperation op first second)
            _ -> (False, undefined)

parseExpressionWith :: [Operator] -> [Token] -> (Expression, [Token])
parseExpressionWith operators tokens = parseOperationWith first operators restTokens
    where (first, restTokens) = parseCall tokens

parseExpression = parseExpressionWith [Plus, Minus, Multiply, Divide, IntDivide, Modulo, And, Or, Xor, RaiseToThePowerOf,
    Assign, Equal, NotEqual, GreaterThan, LowerThan, GreaterThanOrEqualTo, LowerThanOrEqualTo, ShiftLeft, ShiftRight]

parseBlockWith :: [a] -> ([Token] -> (a, [Token])) -> [Token] -> [Token] -> ([a], Token, [Token])
parseBlockWith result parser ends (token : tokens)
    | token `elem` ends = (reverse result, token, tokens)
    | otherwise = parseBlockWith (parsed : result) parser ends restTokens
    where (parsed, restTokens) = parser $ token : tokens

parseBlock = parseBlockWith []

parseIf :: [(Expression, [Expression])] -> [Token] -> (Expression, [Token])
parseIf ifs tokens = case blockTerminator of
    Keyword Else _ -> (IfExpression (reverse allIfs) elseBlock, tokensAfterElse)
    Keyword End _ -> (IfExpression (reverse allIfs) [], tokensAfterBlock)
    Keyword Elif _ -> parseIf allIfs tokensAfterBlock
    where
        (condition, tokensAfterCondition) = parseExpression tokens
        (block, blockTerminator, tokensAfterBlock) = parseBlock parseExpression [Keyword Else "", Keyword Elif "", Keyword End ""] tokensAfterCondition
        (elseBlock, _, tokensAfterElse) = parseBlock parseExpression [Keyword End ""] tokensAfterBlock
        allIfs = (condition, block) : ifs

parseWhile :: [Token] -> (Expression, [Token])
parseWhile tokens = (WhileExpression conditionExpression blockExpressions, tokensAfterBlock)
    where
        (conditionExpression, tokensAfterCondition) = parseExpression tokens
        (blockExpressions, _, tokensAfterBlock) = parseBlock parseExpression [Keyword End ""] tokensAfterCondition

parseFunction :: [Token] -> (Expression, [Token])
parseFunction (Identifier name : Operator BracketOpen _ : tokens) = (Function name args block, tokensAfterBlock)
    where
        parseIdentifier (Identifier name : tokens) = (name, tokens)
        (args, _, tokensAfterArgs) = parseBlock parseIdentifier [Operator BracketClose ""] tokens
        (block, _, tokensAfterBlock) = parseBlock parseExpression [Keyword End ""] tokensAfterArgs

parseFile :: [Expression] -> [Token] -> [Expression]
parseFile previous [] = reverse previous
parseFile previous tokens = parseFile (expression : previous) restTokens
    where (expression, restTokens) = parseExpression tokens

parse = parseFile []