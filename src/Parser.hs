module Parser (Expression (..), Statement (..), Declaration (..), parse) where

import Data.Ratio ((%))
import qualified Lexer
import Functions (pair, join, tailRecM, tailRec2M, fmapFst)
import Errors (Fallible, failure, assert)

data Declaration = Declaration String (Integer, Expression) deriving (Show, Eq)
data Statement
  = Expression (Integer, Expression)
  | Assignment String (Integer, Expression)
  | While (Integer, Expression) [(Integer, Statement)]
  | IfChain [((Integer, Expression), [(Integer, Statement)])] [(Integer, Statement)] deriving (Show, Eq)

data Expression
  = LiteralInt Integer
  | LiteralFloat Double
  | LiteralText String
  | LiteralBool Bool
  | Name String
  | Call (Integer, Expression) [(Integer, Expression)]
  | Primitive String [(Integer, Expression)]
  | Field (Integer, Expression) String
  | Lambda [(String, (Integer, Expression))] (Integer, Expression) [(Integer, Statement)] deriving (Show, Eq)

expect :: Lexer.Token -> [(Integer, Lexer.Token)] -> Fallible [(Integer, Lexer.Token)]
expect e [] = failure (-1) $ "Expected " ++ show e ++ ", reached the EOF"
expect e ((line, l) : ts) = if l == e then return ts else failure line $ "Expected " ++ show e ++ ", got " ++ show l

parseMany :: ([(Integer, Lexer.Token)] -> Fallible (a, [(Integer, Lexer.Token)])) -> Lexer.Token -> [(Integer, Lexer.Token)] -> Fallible ([a], [(Integer, Lexer.Token)])
parseMany parser end tokens = tailRec2M if' reverse tail else' [] tokens
  where if' result tokens = assert (not $ null tokens) (-1) "Unexpected end of file" >> Right (snd (head tokens) == end)
        else' result tokens = fmapFst (: result) $ parser tokens

parseValue :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseValue ((line, Lexer.LiteralInt _ lit) : tokens) = Right ((line, LiteralInt lit), tokens)
parseValue ((line, Lexer.LiteralFloat _ n exp) : tokens) = Right ((line, LiteralFloat $ fromIntegral n / (10.0 ** fromIntegral exp)), tokens)
parseValue ((line, Lexer.LiteralText lit) : tokens) = Right ((line, LiteralText lit), tokens)
parseValue ((line, Lexer.LiteralBool lit) : tokens) = Right ((line, LiteralBool lit), tokens)
parseValue ((line, Lexer.Word "fun") : tokens) = parseLambda line tokens
parseValue ((line, Lexer.Word name) : tokens) = Right ((line, Name name), tokens)
parseValue ((line, Lexer.Separator '(') : tokens) = do
  (expression, tokensAfterExpression) <- parseExpression tokens
  tokensAfterParenthesis <- expect (Lexer.Separator ')') tokensAfterExpression
  Right (expression, tokensAfterParenthesis)
parseValue ((line, token) : _) = failure line $ "Expected a value but got " ++ show token

parseArguments :: [(Integer, Lexer.Token)] -> Fallible ([(String, (Integer, Expression))], [(Integer, Lexer.Token)])
parseArguments tokens = tailRecM if' then' else' ([], [], tokens)
  where if' (_, _, tokens) = assert (not $ null tokens) (-1) "Unexpected end of file" >> Right (snd (head tokens) == Lexer.Separator ']')
        then' (names, args, (line, _) : tokens) = assert (null names) line ("Missing type for " ++ join ", " names) >> Right (reverse args, tokens)
        else' (names, args, (_, Lexer.Word name) : tokens) = Right (name : names, args, tokens)
        else' (names, args, (_, Lexer.Separator ':') : tokens) = do
          (type', restTokens) <- parseExpression tokens
          Right ([], map (`pair` type') names ++ args, restTokens)
        else' (names, args, (line, token) : _) = failure line $ "Unexpected " ++ show token ++ " in the argument list"

parseLambda :: Integer -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseLambda line tokens = do
  tokensAfterBracket <- expect (Lexer.Separator '[') tokens
  (args, tokensAfterArgs) <- parseArguments tokensAfterBracket
  (returnType, tokensAfterReturnType) <- parseExpression tokensAfterArgs
  (block, tokensAfterBlock) <- parseBlock tokensAfterReturnType
  Right ((line, Lambda args returnType block), tokensAfterBlock)

parseCall :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseCall tokens = parseValue tokens >>= uncurry (tailRec2M if' id id else')
  where if' fun tokens = Right $ null tokens || snd (head tokens) `notElem` [Lexer.Separator '[', Lexer.Separator '.', Lexer.Separator ':']
        else' fun ((line, Lexer.Separator '[') : tokens) = fmapFst (pair line . Call fun) $ parseMany parseExpression (Lexer.Separator ']') tokens
        else' obj ((line, Lexer.Separator '.') : tokens) = case tokens of
          (_, Lexer.Word name) : (_, Lexer.Separator '[') : restTokens -> fmapFst (pair line . Primitive name . (obj :)) $ parseMany parseExpression (Lexer.Separator ']') restTokens
          _ -> failure line $ "Expected a primitive name followed by ["
        else' obj ((line, Lexer.Separator ':') : tokens) = case tokens of
          (_, Lexer.Word name) : restTokens -> Right ((line, Field obj name), restTokens)
          _ -> failure line $ "Expected a field name"

parseExpression :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseExpression tokens = parseCall tokens >>= uncurry (tailRec2M if' id id else')
  where if' first tokens = Right (case tokens of (_, Lexer.Operator _) : _ -> False; _ -> True)
        else' first ((line, Lexer.Operator op) : tokens) = fmapFst (\second -> (line, Primitive op [first, second])) $ parseCall tokens

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

parseStatement ((line, Lexer.Word name) : (_, Lexer.Operator "=") : tokens) = do
  (expression, restTokens) <- parseExpression tokens
  Right ((line, Assignment name expression), restTokens)

parseStatement tokens@((line, _):_) = fmap (\(expression, restTokens) -> ((line, Expression expression), restTokens)) $ parseExpression tokens

parseBlock :: [(Integer, Lexer.Token)] -> Fallible ([(Integer, Statement)], [(Integer, Lexer.Token)])
parseBlock tokens = expect (Lexer.Separator '{') tokens >>= parseMany parseStatement (Lexer.Separator '}')

parseDeclaration :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Declaration), [(Integer, Lexer.Token)])
parseDeclaration ((line, Lexer.Word name) : tokens) = do
  (expression, restTokens) <- parseExpression tokens
  Right ((line, Declaration name expression), restTokens)
parseDeclaration ((line, token) : _) = failure line $ "Expected a name of declared function but got " ++ show token

parse :: [(Integer, Lexer.Token)] -> Fallible [(Integer, Declaration)]
parse = tailRecM (Right . null . snd) (Right . reverse . fst) else' . pair []
  where else' (result, tokens) = fmapFst (: result) $ parseDeclaration tokens
