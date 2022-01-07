module Parser (Expression (..), Statement (..), Declaration (..), File (..), parse) where

import Data.Ratio ((%))
import qualified Lexer
import qualified Data.Map.Strict as Map
import Functions (intercalate, pair, tailRecM, tailRec2M, fmap2, follow)
import Fallible (Fallible (..), failure, assert)

newtype File = File [(Integer, Declaration)] deriving (Show, Eq)

data Declaration
  = Declaration String (Integer, Expression)
  | Import String String
  | Php String (Map.Map String (Integer, Expression)) deriving (Show, Eq)

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
  | LiteralRecord (Map.Map String (Integer, Expression))
  | Name String
  | Call (Integer, Expression) [(Integer, Expression)]
  | Access (Integer, Expression) String (Maybe [(Integer, Expression)])
  | Lambda [(String, (Integer, Expression))] (Integer, Expression) [(Integer, Statement)] deriving (Show, Eq)

expect :: Lexer.Token -> [(Integer, Lexer.Token)] -> Fallible [(Integer, Lexer.Token)]
expect e [] = failure (-1) $ "Expected " ++ show e ++ ", reached the EOF"
expect e ((line, l) : ts) = if l == e then return ts else failure line $ "Expected " ++ show e ++ ", got " ++ show l

parseMany :: ([(Integer, Lexer.Token)] -> Fallible (a, [(Integer, Lexer.Token)])) -> Lexer.Token -> [(Integer, Lexer.Token)] -> Fallible ([a], [(Integer, Lexer.Token)])
parseMany parser end tokens = tailRec2M if' reverse tail else' [] tokens
  where if' result tokens = assert (not $ null tokens) (-1) "Unexpected end of file" >> Ok (snd (head tokens) == end)
        else' result tokens = fmap2 (: result) id $ parser tokens

parseValue :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseValue ((line, Lexer.LiteralInt _ lit) : tokens) = Ok ((line, LiteralInt lit), tokens)
parseValue ((line, Lexer.LiteralFloat _ n exp) : tokens) = Ok ((line, LiteralFloat $ fromIntegral n / (10.0 ** fromIntegral exp)), tokens)
parseValue ((line, Lexer.LiteralText lit) : tokens) = Ok ((line, LiteralText lit), tokens)
parseValue ((line, Lexer.LiteralBool lit) : tokens) = Ok ((line, LiteralBool lit), tokens)
parseValue ((line, Lexer.Word "fun") : tokens) = parseLambda line tokens
parseValue ((line, Lexer.Word name) : tokens) = Ok ((line, Name name), tokens)
parseValue ((line, Lexer.Separator '(') : tokens) = do
  (expression, tokensAfterExpression) <- parseExpression tokens
  tokensAfterParenthesis <- expect (Lexer.Separator ')') tokensAfterExpression
  Ok (expression, tokensAfterParenthesis)
parseValue ((line, Lexer.Separator '{') : tokens) = parseRecord line tokens
parseValue ((line, token) : _) = failure line $ "Expected a value but got " ++ show token

parseFields :: Map.Map String (Integer, Expression) -> [(Integer, Lexer.Token)] -> Fallible (Map.Map String (Integer, Expression), [(Integer, Lexer.Token)])
parseFields fields ((line, Lexer.Word name) : tokens) = do
  assert (not $ Map.member name fields) line $ "Duplicate field " ++ name
  (expression, restTokens) <- parseExpression tokens
  parseFields (Map.insert name expression fields) restTokens
parseFields fields ((_, Lexer.Separator '}') : tokens) = Ok (fields, tokens)
parseFields fields ((line, token) : _) = failure line $ "Expected a field name but got " ++ show token

parseRecord :: Integer -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseRecord line tokens = do
  (fields, tokensAfterFields) <- parseFields Map.empty tokens
  Ok ((line, LiteralRecord fields), tokensAfterFields)

parseArguments :: [(Integer, Lexer.Token)] -> Fallible ([(String, (Integer, Expression))], [(Integer, Lexer.Token)])
parseArguments tokens = tailRecM if' then' else' ([], [], tokens)
  where if' (_, _, tokens) = assert (not $ null tokens) (-1) "Unexpected end of file" >> Ok (snd (head tokens) == Lexer.Separator ']')
        then' (names, args, (line, _) : tokens) = assert (null names) line ("Missing type for " ++ intercalate ", " names) >> Ok (reverse args, tokens)
        else' (names, args, (_, Lexer.Word name) : tokens) = Ok (name : names, args, tokens)
        else' (names, args, (_, Lexer.Separator ':') : tokens) = do
          (type', restTokens) <- parseExpression tokens
          Ok ([], map (`pair` type') names ++ args, restTokens)
        else' (names, args, (line, token) : _) = failure line $ "Unexpected " ++ show token ++ " in the argument list"

parseLambda :: Integer -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseLambda line tokens = do
  tokensAfterBracket <- expect (Lexer.Separator '[') tokens
  ((args, (returnType, block)), restTokens) <- follow parseArguments (follow parseExpression parseBlock) tokensAfterBracket
  Ok ((line, Lambda args returnType block), restTokens)

parseCall :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseCall tokens = parseValue tokens >>= uncurry (tailRec2M if' id id else')
  where if' fun tokens = Ok $ null tokens || snd (head tokens) `notElem` [Lexer.Separator '[', Lexer.Separator '.', Lexer.Separator ':']
        else' fun ((line, Lexer.Separator '[') : tokens) = fmap2 (pair line . Call fun) id $ parseExpressions tokens
        else' obj ((line, Lexer.Separator '.') : (_, Lexer.Word name) : (_, Lexer.Separator '[') : tokens) = fmap2 (pair line . Access obj name . Just) id $ parseExpressions tokens
        else' obj ((line, Lexer.Separator '.') : (_, Lexer.Word name) : tokens) = Ok ((line, Access obj name Nothing), tokens)
        else' obj ((line, Lexer.Separator c) : _) = failure line $ "Expected field or primitive name after " ++ [c]
        parseExpressions = parseMany parseExpression $ Lexer.Separator ']'

parseExpression :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Expression), [(Integer, Lexer.Token)])
parseExpression tokens = parseCall tokens >>= uncurry (tailRec2M if' id id else')
  where if' first tokens = Ok (case tokens of (_, Lexer.Operator _) : _ -> False; _ -> True)
        else' first ((line, Lexer.Operator op) : tokens) = fmap2 (\second -> (line, Access first op $ Just [second])) id $ parseCall tokens

parseIf :: Integer -> [((Integer, Expression), [(Integer, Statement)])] -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Statement), [(Integer, Lexer.Token)])
parseIf line chain tokens = do
  (pair, tokensAfterBlock) <- follow parseExpression parseBlock tokens
  let newChain = pair : chain
  case tokensAfterBlock of
    (_, Lexer.Word "else") : (_, Lexer.Word "if") : restTokens -> parseIf line newChain restTokens
    (_, Lexer.Word "else") : restTokens -> do
      (elseBlock, tokensAfterElse) <- parseBlock restTokens
      Ok ((line, IfChain (reverse newChain) $ elseBlock), tokensAfterElse)
    restTokens -> Ok ((line, IfChain (reverse newChain) []), restTokens)

parseStatement :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Statement), [(Integer, Lexer.Token)])
parseStatement ((line, Lexer.Word "if") : tokens) = parseIf line [] tokens
parseStatement ((line, Lexer.Word "while") : tokens) = do
  ((condition, block), restTokens) <- follow parseExpression parseBlock tokens
  Ok ((line, While condition block), restTokens)

parseStatement ((line, Lexer.Word name) : (_, Lexer.Operator "=") : tokens) = do
  (expression, restTokens) <- parseExpression tokens
  Ok ((line, Assignment name expression), restTokens)

parseStatement tokens@((line, _):_) = fmap2 (pair line . Expression) id $ parseExpression tokens

parseBlock :: [(Integer, Lexer.Token)] -> Fallible ([(Integer, Statement)], [(Integer, Lexer.Token)])
parseBlock tokens = expect (Lexer.Separator '{') tokens >>= parseMany parseStatement (Lexer.Separator '}')

parseDeclaration :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Declaration), [(Integer, Lexer.Token)])
parseDeclaration ((line, Lexer.Word "import") : (_, Lexer.Word "php") : (_, Lexer.LiteralText path) : tokens) = do
  tokensAfterBracket <- expect (Lexer.Separator '{') tokens
  (imports, tokensAfterImports) <- parseFields Map.empty tokensAfterBracket
  Ok ((line, Php path imports), tokensAfterImports)

parseDeclaration ((line, Lexer.Word "import") : (_, Lexer.Word name) : (_, Lexer.LiteralText path) : tokens) = Ok ((line, Import name path), tokens)

parseDeclaration ((line, Lexer.Word name) : tokens) = do
  (expression, restTokens) <- parseExpression tokens
  Ok ((line, Declaration name expression), restTokens)
parseDeclaration ((line, token) : _) = failure line $ "Expected a name of declared function but got " ++ show token

parse :: [(Integer, Lexer.Token)] -> Fallible File
parse = tailRecM (Ok . null . snd) (Ok . File . reverse . fst) else' . pair []
  where else' (result, tokens) = fmap2 (: result) id $ parseDeclaration tokens
