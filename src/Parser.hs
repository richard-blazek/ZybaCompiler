module Parser (Expression (..), Declaration (..), parse) where

import Lexer (Lexeme (..), Token)
import Data.Ratio ((%))
import Functions (pair)
import Errors (Fallible, failure)

data Declaration = Function Integer String [String] Expression deriving (Show, Read, Eq)
data Expression
    = Integer Integer Integer
    | Rational Integer Rational
    | String Integer String
    | Name Integer String
    | Operation Expression [Expression]
    | Condition Expression Expression Expression
    | Assignment String Expression Expression deriving (Show, Read, Eq)

expect :: [Lexeme] -> ([Token] -> Fallible (t, [Token])) -> [Token] -> Fallible (t, [Token])
expect [] success tokens = success tokens
expect (expected : following) success [] = failure 0 $ ": Expected " ++ show expected ++ " but reached the EOF"
expect (expected : following) success ((line, lexeme) : tokens)
    | lexeme == expected = expect following success tokens
    | otherwise = failure line $ "Expected " ++ show expected ++ " but got " ++ show lexeme

parseMany :: [a] -> ([Token] -> Fallible (a, [Token])) -> Lexeme -> [Token] -> Fallible ([a], [Token])
parseMany result parser end all@((_, lexeme) : tokens)
    | lexeme == end = Right (reverse result, tokens)
    | otherwise = do
        (parsed, restTokens) <- parser all
        parseMany (parsed : result) parser end restTokens

parseValue :: [Token] -> Fallible (Expression, [Token])
parseValue ((line, LiteralInteger _ i) : tokens) = Right (Integer line i, tokens)
parseValue ((line, LiteralRational _ n exp) : tokens) = Right (Rational line $ n % (10 ^ exp), tokens)
parseValue ((line, LiteralString s) : tokens) = Right (String line s, tokens)
parseValue ((line, Word "if") : tokens) = parseCondition tokens
parseValue ((line, Word "for") : tokens) = parseAssignment tokens
parseValue ((line, Word name) : tokens) = Right (Name line name, tokens)
parseValue ((line, Separator '(') : tokens) = do
    (expression, restTokens) <- parseExpression tokens
    expect [Separator ')'] (Right . (pair expression)) restTokens
parseValue ((line, lexeme) : tokens) = failure line $ "Expected a value but got " ++ show lexeme

parseCondition :: [Token] -> Fallible (Expression, [Token])
parseCondition tokens = do
    (condition, tokensAfterCondition) <- parseExpression tokens
    (ifTrue, tokensAfterThen) <- parseExpression tokensAfterCondition
    (ifFalse, tokensAfterElse) <- parseExpression tokensAfterThen
    Right (Condition condition ifTrue ifFalse, tokensAfterElse)

parseAssignment :: [Token] -> Fallible (Expression, [Token])
parseAssignment ((_, Word name) : (_, Operator "=") : tokens) = do
    (assigned, tokensAfterAssignment) <- parseExpression tokens
    (expression, tokensAfterExpression) <- parseExpression tokensAfterAssignment
    Right (Assignment name assigned expression, tokensAfterExpression)
    
parseAssignment ((line, Word name) : tokens) = failure line $ "Expected '=' when assigning to " ++ name
parseAssignment ((line, token) : tokens) = failure line $ "Expected a Name name but got " ++ show token

parseBrackets :: Expression -> [Token] -> Fallible (Expression, [Token])
parseBrackets fun ((_, Separator '[') : tokens) = do
    (args, restTokens) <- parseMany [] parseExpression (Separator ']') tokens
    parseBrackets (Operation fun args) restTokens
parseBrackets fun tokens = Right (fun, tokens)

parseCall :: [Token] -> Fallible (Expression, [Token])
parseCall tokens = parseValue tokens >>= uncurry parseBrackets

parseOperation :: Expression -> [Token] -> Fallible (Expression, [Token])
parseOperation first ((line, Operator op) : tokens) = do
    (second, restTokens) <- parseCall tokens
    parseOperation (Operation (Name line op) [first, second]) restTokens
parseOperation first tokens = Right (first, tokens)

parseExpression :: [Token] -> Fallible (Expression, [Token])
parseExpression tokens = parseCall tokens >>= uncurry parseOperation

parseFunction :: Integer -> String -> [Token] -> Fallible (Declaration, [Token])
parseFunction line name tokens = do
    (args, tokensAfterArgs) <- parseMany [] parseWord (Separator ']') tokens
    (expression, tokensAfterExpression) <- parseExpression tokensAfterArgs
    Right (Function line name args expression, tokensAfterExpression)
    where
        parseWord ((_, Word n) : tokens) = Right (n, tokens)
        parseWord _ = failure line "Expected an argument name"

parseDeclaration :: [Token] -> Fallible (Declaration, [Token])
parseDeclaration ((line, lexeme) : tokens) = case lexeme of
    Word name -> expect [Word "is", Word "fun", Separator '['] (parseFunction line name) tokens
    _ -> failure line $ "Expected a name but got " ++ show lexeme

parseDeclarations :: [Declaration] -> [Token] -> Fallible [Declaration]
parseDeclarations result [] = Right $ reverse result
parseDeclarations result tokens = do
    (decl, restTokens) <- parseDeclaration tokens
    parseDeclarations (decl : result) restTokens

parse = parseDeclarations []