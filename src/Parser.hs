module Parser (Expression (..), Statement (..), Declaration (..), parse) where

import Data.Ratio ((%))
import qualified Lexer
import Functions (pair, join)
import Errors (Fallible, failure)

data Declaration = Declaration String (Integer, Expression) deriving (Show, Read, Eq)
data Statement
  = Expression (Integer, Expression)
  | Assignment String (Integer, Expression)
  | While (Integer, Expression) [(Integer, Statement)]
  | IfChain [((Integer, Expression), [(Integer, Statement)])] [(Integer, Statement)] deriving (Show, Read, Eq)

data Expression
  = Integer Integer
  | Rational Rational
  | String String
  | Name String
  | Call (Integer, Expression) [(Integer, Expression)]
  | Lambda [(String, (Integer, Expression))] (Integer, Expression) [(Integer, Statement)] deriving (Show, Read, Eq)

expect :: Lexer.Token -> [(Integer, Lexer.Token)] -> Fallible [(Integer, Lexer.Token)]
expect e [] = failure (-1) $ "Expected " ++ show e ++ ", reached the EOF"
expect e ((line, l) : ts) = if l == e then return ts else failure line $ "Expected " ++ show e ++ ", got " ++ show l

parseMany :: [a] -> ([(Integer, Lexer.Token)] -> Fallible (a, [(Integer, Lexer.Token)])) -> Lexer.Token -> [(Integer, Lexer.Token)] -> Fallible ([a], [(Integer, Lexer.Token)])
parseMany result parser end all@((_, lexeme) : tokens)
  | lexeme == end = Right (reverse result, tokens)
  | otherwise = do
    (parsed, restTokens) <- parser all
    parseMany (parsed : result) parser end restTokens
parseMany _ _ _ [] = failure (-1) "Unexpected end of file"

parseValue :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseValue ((line, Lexer.LiteralInteger _ i) : tokens) = Right ((line, Integer i), tokens)
parseValue ((line, Lexer.LiteralRational _ n exp) : tokens) = Right ((line, Rational $ n % (10 ^ exp)), tokens)
parseValue ((line, Lexer.LiteralString s) : tokens) = Right ((line, String s), tokens)
parseValue ((line, Lexer.Word "fun") : tokens) = parseLambda line tokens
parseValue ((line, Lexer.Word name) : tokens) = Right ((line, Name name), tokens)
parseValue ((line, Lexer.Separator '(') : tokens) = do
  (expression, tokensAfterExpression) <- parseExpression tokens
  tokensAfterParenthesis <- expect (Lexer.Separator ')') tokensAfterExpression
  Right (expression, tokensAfterParenthesis)
parseValue ((line, lexeme) : _) = failure line $ "Expected a value but got " ++ show lexeme

parseArguments :: [String] -> [(String, (Integer, Expression))] -> [(Integer, Lexer.Token)] -> Fallible ([(String, (Integer, Expression))], [(Integer, Lexer.Token)])
parseArguments names args ((_, Lexer.Word name) : tokens) = parseArguments (name : names) args tokens
parseArguments [] args ((_, Lexer.Separator ']') : tokens) = Right (reverse args, tokens)
parseArguments names args ((line, Lexer.Separator ']') : tokens) = failure line $ "Argument list closed without any type specified for arguments " ++ join "," names
parseArguments names args ((line, Lexer.Separator ':') : tokens) = do
  (argType, tokensAfterType) <- parseExpression tokens
  parseArguments [] (map (`pair` argType) names ++ args) tokensAfterType

parseArguments _ _ ((line, lexeme) : _) = failure line $ "Expected an argument name, got " ++ show lexeme
parseArguments _ _ [] = failure (-1) "Unexpected end of file"

parseLambda :: Integer -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseLambda line tokens = do
  tokensAfterBracket <- expect (Lexer.Separator '[') tokens
  (args, tokensAfterArgs) <- parseArguments [] [] tokensAfterBracket
  (returnType, tokensAfterReturnType) <- parseExpression tokensAfterArgs
  (block, tokensAfterBlock) <- parseBlock tokensAfterReturnType
  Right ((line, Lambda args returnType block), tokensAfterBlock)

parseBrackets :: (Integer, Expression) -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseBrackets fun ((line, Lexer.Separator '[') : tokens) = do
  (args, restTokens) <- parseMany [] parseExpression (Lexer.Separator ']') tokens
  parseBrackets (line, Call fun args) restTokens
parseBrackets fun tokens = Right (fun, tokens)

parseCall :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseCall tokens = parseValue tokens >>= uncurry parseBrackets

parseOperation :: (Integer, Expression) -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseOperation first ((line, Lexer.Operator op) : tokens) = do
  (second, restTokens) <- parseCall tokens
  parseOperation (line, Call (line, Name op) [first, second]) restTokens
parseOperation first tokens = Right (first, tokens)

parseExpression :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseExpression tokens = parseCall tokens >>= uncurry parseOperation

parseIf :: Integer -> [((Integer, Expression), [(Integer, Statement)])] -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Statement), [(Integer, Lexer.Token)])
parseIf line chain tokens = do
  (condition, tokensAfterCondition) <- parseExpression tokens
  (thenBlock, tokensAfterThen) <- parseBlock tokensAfterCondition
  let newChain = (condition, thenBlock) : chain
  case tokensAfterThen of
    (_, Lexer.Word "else") : (_, Lexer.Word "if") : restTokens -> parseIf line newChain restTokens
    (_, Lexer.Word "else") : restTokens -> do
      (elseBlock, tokensAfterElse) <- parseBlock restTokens
      Right ((line, IfChain (reverse newChain) $ elseBlock), tokensAfterElse)
    restTokens -> Right ((line, IfChain (reverse newChain) []), restTokens)

parseStatement :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Statement), [(Integer, Lexer.Token)])
parseStatement ((line, Lexer.Word "if") : tokens) = parseIf line [] tokens
parseStatement ((line, Lexer.Word "while") : tokens) = do
  (condition, tokensAfterCondition) <- parseExpression tokens
  (block, tokensAfterBlock) <- parseBlock tokensAfterCondition
  Right ((line, While condition block), tokensAfterBlock)

parseStatement ((line, Lexer.Word name) : (_, Lexer.Separator ':') : tokens) = do
  (expression, restTokens) <- parseExpression tokens
  Right ((line, Assignment name expression), restTokens)

parseStatement tokens@((line, _):_) = fmap (\(expression, restTokens) -> ((line, Expression expression), restTokens)) $ parseExpression tokens

parseBlock :: [(Integer, Lexer.Token)] -> Fallible ([(Integer, Statement)], [(Integer, Lexer.Token)])
parseBlock tokens = expect (Lexer.Separator '(') tokens >>= parseMany [] parseStatement (Lexer.Separator ')')

parseDeclaration :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Declaration), [(Integer, Lexer.Token)])
parseDeclaration ((line, Lexer.Word name) : tokens) = do
  (expression, restTokens) <- parseExpression tokens
  Right ((line, Declaration name expression), restTokens)
parseDeclaration ((line, lexeme) : _) = failure line $ "Expected a name of declared function but got " ++ show lexeme

parseDeclarations :: [(Integer, Declaration)] -> [(Integer, Lexer.Token)] -> Fallible [(Integer, Declaration)]
parseDeclarations result [] = Right $ reverse result
parseDeclarations result tokens = do
  (decl, restTokens) <- parseDeclaration tokens
  parseDeclarations (decl : result) restTokens

parse = parseDeclarations []