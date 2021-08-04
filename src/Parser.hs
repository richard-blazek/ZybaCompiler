module Parser (Expression (..), parse) where

import Lexer (Token (..), Operator (..), Keyword (..))

data Expression = Integer Integer
                | Rational Rational
                | String String
                | Variable String
                | FunctionCall Expression [Expression]
                | BinaryOperation Operator Expression Expression
                | IfExpression [(Expression, [Expression])] [Expression]
                | WhileExpression Expression [Expression]
                | Function String [String] [Expression] deriving (Show, Eq)

parseValue :: [Token] -> (Expression, [Token])
parseValue (LiteralInteger i : tokens) = (Integer i, tokens)
parseValue (LiteralDecimal d : tokens) = (Rational d, tokens)
parseValue (LiteralString s : tokens) = (String s, tokens)
parseValue (Identifier i : tokens) = (Variable i, tokens)
parseValue (Operator ParenthesisOpen : tokens) = case tokensAfterExpression of
    Operator ParenthesisClose : restTokens -> (expression, restTokens)
    _ -> error "Expected parenthesis close"
    where (expression, tokensAfterExpression) = parseExpression tokens
parseValue (Keyword If : tokens) = parseIf [] tokens
parseValue (Keyword While : tokens) = parseWhile tokens
parseValue (Keyword Fun : tokens) = parseFunction tokens

parseCallArgs :: Expression -> [Token] -> (Expression, [Token])
parseCallArgs func (Operator BracketOpen : tokens) = parseCallArgs (FunctionCall func args) tokensAfterArgs
    where (args, _, tokensAfterArgs) = parseBlock [Operator BracketClose] [] tokens
parseCallArgs func tokens = (func, tokens)

parseCall :: [Token] -> (Expression, [Token])
parseCall tokens = parseCallArgs func restTokens
    where (func, restTokens) = parseValue tokens

parseOperationWith :: ([Token] -> (Expression, [Token])) -> [Operator] -> Expression -> [Token] -> (Expression, [Token])
parseOperationWith parseFn operators first tokens
    | isOperator = parseOperationWith parseFn operators operation restTokens
    | otherwise = (first, tokens)
    where
        (second, restTokens) = parseFn (tail tokens)
        (isOperator, operation) = case tokens of
            Operator op : _ -> (op `elem` operators, BinaryOperation op first second)
            _ -> (False, undefined)

parseExpressionWith :: ([Token] -> (Expression, [Token])) -> [Operator] -> [Token] -> (Expression, [Token])
parseExpressionWith parseFn operators tokens = parseOperationWith parseFn operators first restTokens
    where (first, restTokens) = parseFn tokens

parseExpression = parseExpressionWith parseCall [Plus, Minus, Multiply, Divide, IntDivide, Modulo, And, Or, Xor, RaiseToThePowerOf,
    Assign, Equal, NotEqual, GreaterThan, LowerThan, GreaterThanOrEqualTo, LowerThanOrEqualTo, ShiftLeft, ShiftRight]

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
parseFunction (Identifier name : Operator BracketOpen : tokens) = (Function name args block, tokensAfterBlock)
    where
        parseArgList args (Operator BracketClose : tokens) = (reverse args, tokens)
        parseArgList args (Identifier name : tokens) = parseArgList (name : args) tokens
        (args, tokensAfterArgs) = parseArgList [] tokens
        (block, _, tokensAfterBlock) = parseBlock [Keyword End] [] tokensAfterArgs

parseFile :: [Expression] -> [Token] -> [Expression]
parseFile previous [] = reverse previous
parseFile previous tokens = parseFile (expression : previous) restTokens
    where (expression, restTokens) = parseExpression tokens

parse = parseFile []