module Parser (Expression (..), Statement (..), Declaration (..), Lined, parse) where

import Lexer (Lexeme (..), Token)
import Data.Ratio ((%))
import Data.Maybe (maybeToList)
import Functions (pair, join)
import Errors (Fallible, failure)

type Lined t = (Integer, t)

data Declaration = Declaration String (Lined Expression)
data Statement
  = Evaluation (Lined Expression)
  | Assignment String (Lined Expression)
  | While (Lined Expression) [Lined Statement]
  | IfChain [(Lined Expression, [Lined Statement])] [Lined Statement] deriving (Show, Read, Eq)

data Expression
  = Integer Integer
  | Rational Rational
  | String String
  | Name String
  | Operation (Lined Expression) [Lined Expression]
  | Function [(String, Lined Expression)] (Lined Expression) [Lined Statement] deriving (Show, Read, Eq)

expect :: Lexeme -> [Token] -> Fallible [Token]
expect e [] = failure (-1) $ "Expected " ++ show e ++ ", reached the EOF"
expect e ((line, l) : ts) = if l == e then return ts else failure line $ "Expected " ++ show e ++ ", got " ++ show l

parseMany :: [a] -> ([Token] -> Fallible (a, [Token])) -> Lexeme -> [Token] -> Fallible ([a], [Token])
parseMany result parser end all@((_, lexeme) : tokens)
  | lexeme == end = Right (reverse result, tokens)
  | otherwise = do
    (parsed, restTokens) <- parser all
    parseMany (parsed : result) parser end restTokens
parseMany _ _ _ [] = failure (-1) "Unexpected end of file"

parseValue :: [Token] -> Fallible (Lined Expression, [Token])
parseValue ((line, LiteralInteger _ i) : tokens) = Right ((line, Integer i), tokens)
parseValue ((line, LiteralRational _ n exp) : tokens) = Right ((line, Rational $ n % (10 ^ exp)), tokens)
parseValue ((line, LiteralString s) : tokens) = Right ((line, String s), tokens)
parseValue ((line, Word "fun") : tokens) = parseFunction line tokens
parseValue ((line, Word name) : tokens) = Right ((line, Name name), tokens)
parseValue ((line, Separator '(') : tokens) = do
  (expression, tokensAfterExpression) <- parseExpression tokens
  tokensAfterParenthesis <- expect (Separator ')') tokensAfterExpression
  Right (expression, tokensAfterParenthesis)
parseValue ((line, lexeme) : _) = failure line $ "Expected a value but got " ++ show lexeme

parseArguments :: [String] -> [(String, Lined Expression)] -> [Token] -> Fallible ([(String, Lined Expression)], [Token])
parseArguments names args ((_, Word name) : tokens) = parseArguments (name : names) args tokens
parseArguments [] args ((_, Separator ']') : tokens) = Right (reverse args, tokens)
parseArguments names args ((line, Separator ']') : tokens) = failure line $ "Argument list closed without any type specified for arguments " ++ join "," names
parseArguments names args ((line, Separator ':') : tokens) = do
  (argType, tokensAfterType) <- parseExpression tokens
  parseArguments [] (map (`pair` argType) names ++ args) tokensAfterType

parseArguments _ _ ((line, lexeme) : _) = failure line $ "Expected an argument name, got " ++ show lexeme
parseArguments _ _ [] = failure (-1) "Unexpected end of file"

parseFunction :: Integer -> [Token] -> Fallible (Lined Expression, [Token])
parseFunction line tokens = do
  tokensAfterBracket <- expect (Separator '[') tokens
  (returnType, tokensAfterReturnType) <- parseExpression tokensAfterBracket
  (args, tokensAfterArgs) <- parseArguments [] [] tokensAfterReturnType
  (block, tokensAfterBlock) <- parseBlock tokensAfterArgs
  Right ((line, Function args returnType block), tokensAfterBlock)

parseBrackets :: Lined Expression -> [Token] -> Fallible (Lined Expression, [Token])
parseBrackets fun ((line, Separator '[') : tokens) = do
  (args, restTokens) <- parseMany [] parseExpression (Separator ']') tokens
  parseBrackets (line, Operation fun args) restTokens
parseBrackets fun tokens = Right (fun, tokens)

parseCall :: [Token] -> Fallible (Lined Expression, [Token])
parseCall tokens = parseValue tokens >>= uncurry parseBrackets

parseOperation :: Lined Expression -> [Token] -> Fallible (Lined Expression, [Token])
parseOperation first ((line, Operator op) : tokens) = do
  (second, restTokens) <- parseCall tokens
  parseOperation (line, Operation (line, Name op) [first, second]) restTokens
parseOperation first tokens = Right (first, tokens)

parseExpression :: [Token] -> Fallible (Lined Expression, [Token])
parseExpression tokens = parseCall tokens >>= uncurry parseOperation

parseIf :: Integer -> [(Lined Expression, [Lined Statement])] -> [Token] -> Fallible (Lined Statement, [Token])
parseIf line chain tokens = do
  (condition, tokensAfterCondition) <- parseExpression tokens
  (thenBlock, tokensAfterThen) <- parseBlock tokensAfterCondition
  let newChain = (condition, thenBlock) : chain in
    case tokensAfterThen of
      (_, Word "else") : (_, Word "if") : restTokens -> parseIf line newChain restTokens
      (_, Word "else") : restTokens -> do
        (elseBlock, tokensAfterElse) <- parseBlock restTokens
        Right ((line, IfChain (reverse newChain) $ elseBlock), tokensAfterElse)
      restTokens -> Right ((line, IfChain (reverse newChain) []), restTokens)

parseStatement :: [Token] -> Fallible (Lined Statement, [Token])
parseStatement ((line, Word "if") : tokens) = parseIf line [] tokens
parseStatement ((line, Word "while") : tokens) = do
  (condition, tokensAfterCondition) <- parseExpression tokens
  (block, tokensAfterBlock) <- parseBlock tokensAfterCondition
  Right ((line, While condition block), tokensAfterBlock)

parseStatement ((line, Word name) : (_, Separator ':') : tokens) = do
  (expression, restTokens) <- parseExpression tokens
  Right ((line, Assignment name expression), restTokens)

parseStatement tokens@((line, _):_) = fmap (\(expression, restTokens) -> ((line, Evaluation expression), restTokens)) $ parseExpression tokens

parseBlock :: [Token] -> Fallible ([Lined Statement], [Token])
parseBlock tokens = expect (Separator '(') tokens >>= parseMany [] parseStatement (Separator ')')

parseDeclaration :: [Token] -> Fallible (Lined Declaration, [Token])
parseDeclaration ((line, Word name) : tokens) = do
  (expression, restTokens) <- parseExpression tokens
  Right ((line, Declaration name expression), restTokens)
parseDeclaration ((line, lexeme) : _) = failure line $ "Expected a name of declared function but got " ++ show lexeme

parseDeclarations :: [Lined Declaration] -> [Token] -> Fallible [Lined Declaration]
parseDeclarations result [] = Right $ reverse result
parseDeclarations result tokens = do
  (decl, restTokens) <- parseDeclaration tokens
  parseDeclarations (decl : result) restTokens

parse = parseDeclarations []