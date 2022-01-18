module Parser (Value (..), Statement (..), Declaration (..), Literal (..), File (..), parse) where

import qualified Lexer
import qualified Data.Map.Strict as Map
import Functions (intercalate, tailRecM, tailRec2M, fmap2, follow)
import Fallible (Fallible (..), err, assert)

newtype File = File [(Integer, Bool, Declaration)] deriving (Show, Eq)

data Declaration
  = Declaration String (Integer, Value)
  | Import String String
  | Php String String (Map.Map String (Integer, Value)) deriving (Show, Eq)

data Statement
  = Value (Integer, Value)
  | Assignment String (Integer, Value)
  | While (Integer, Value) [(Integer, Statement)]
  | For String (Maybe String) (Integer, Value) [(Integer, Statement)]
  | IfChain [((Integer, Value), [(Integer, Statement)])] [(Integer, Statement)] deriving (Show, Eq)

data Literal = Int Integer | Real Double | Text String | Bool Bool deriving (Show, Eq)

data Value
  = Literal Literal
  | Record (Map.Map String (Integer, Value))
  | Name [String]
  | Call (Integer, Value) [(Integer, Value)]
  | Operation String [(Integer, Value)]
  | Access (Integer, Value) String
  | Lambda [(String, (Integer, Value))] (Integer, Value) [(Integer, Statement)] deriving (Show, Eq)

expect :: Lexer.Token -> [(Integer, Lexer.Token)] -> Fallible [(Integer, Lexer.Token)]
expect e [] = err (-1) $ "Expected " ++ show e ++ ", reached the EOF"
expect e ((line, l) : ts) = if l == e then return ts else err line $ "Expected " ++ show e ++ ", got " ++ show l

parseMany :: ([(Integer, Lexer.Token)] -> Fallible (a, [(Integer, Lexer.Token)])) -> Lexer.Token -> [(Integer, Lexer.Token)] -> Fallible ([a], [(Integer, Lexer.Token)])
parseMany parser end tokens = tailRec2M if' reverse tail else' [] tokens
  where if' result tokens = assert (not $ null tokens) (-1) "Unexpected end of file" >> Right (snd (head tokens) == end)
        else' result tokens = fmap2 (: result) id $ parser tokens

parseFactor :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Value), [(Integer, Lexer.Token)])
parseFactor ((line, Lexer.LiteralInt _ lit) : tokens) = Right ((line, Literal $ Int lit), tokens)
parseFactor ((line, Lexer.LiteralReal _ n exp) : tokens) = Right ((line, Literal $ Real $ fromIntegral n / (10.0 ** fromIntegral exp)), tokens)
parseFactor ((line, Lexer.LiteralText lit) : tokens) = Right ((line, Literal $ Text lit), tokens)
parseFactor ((line, Lexer.LiteralBool lit) : tokens) = Right ((line, Literal $ Bool lit), tokens)
parseFactor ((line, Lexer.Name "fun") : tokens) = parseLambda line tokens
parseFactor ((line, Lexer.Name name) : tokens) = Right ((line, Name [name]), tokens)
parseFactor ((line, Lexer.Separator '(') : tokens) = do
  (value, tokens') <- parseValue tokens
  fmap ((,) value) $ expect (Lexer.Separator ')') tokens'
parseFactor ((line, Lexer.Separator '{') : tokens) = parseRecord line tokens
parseFactor ((line, token) : _) = err line $ "Expected a value but got " ++ show token

parseFields :: Map.Map String (Integer, Value) -> [(Integer, Lexer.Token)] -> Fallible (Map.Map String (Integer, Value), [(Integer, Lexer.Token)])
parseFields fields ((line, Lexer.Name name) : tokens) = do
  assert (not $ Map.member name fields) line $ "Duplicate field " ++ name
  (value, tokens') <- parseValue tokens
  parseFields (Map.insert name value fields) tokens'
parseFields fields ((_, Lexer.Separator '}') : tokens) = Right (fields, tokens)
parseFields fields ((line, token) : _) = err line $ "Expected a field name but got " ++ show token

parseRecord :: Integer -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Value), [(Integer, Lexer.Token)])
parseRecord line = fmap2 ((,) line . Record) id . parseFields Map.empty

parseArguments :: [(Integer, Lexer.Token)] -> Fallible ([(String, (Integer, Value))], [(Integer, Lexer.Token)])
parseArguments tokens = tailRecM if' then' else' ([], [], tokens)
  where if' (_, _, tokens) = assert (not $ null tokens) (-1) "Unexpected end of file" >> Right (snd (head tokens) == Lexer.Separator ']')
        then' (names, args, (line, _) : tokens) = assert (null names) line ("Missing type for " ++ intercalate ", " names) >> Right (reverse args, tokens)
        else' (names, args, (_, Lexer.Name name) : tokens) = Right (name : names, args, tokens)
        else' (names, args, (_, Lexer.Separator ':') : tokens) = do
          (type', tokens') <- parseValue tokens
          Right ([], map (flip (,) type') names ++ args, tokens')
        else' (names, args, (line, token) : _) = err line $ "Unexpected " ++ show token ++ " in the argument list"

parseLambda :: Integer -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Value), [(Integer, Lexer.Token)])
parseLambda line tokens = do
  tokens' <- expect (Lexer.Separator '[') tokens
  ((args, (returnType, block)), tokens'') <- follow parseArguments (follow parseValue parseBlock) tokens'
  Right ((line, Lambda args returnType block), tokens'')

parseCall :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Value), [(Integer, Lexer.Token)])
parseCall tokens = parseFactor tokens >>= uncurry (tailRec2M if' id id else')
  where if' fun tokens = Right $ null tokens || snd (head tokens) `notElem` [Lexer.Separator '[', Lexer.Separator '.', Lexer.Separator ':']
        else' fun ((line, Lexer.Separator '[') : tokens) = fmap2 ((,) line . Call fun) id $ parseValues tokens
        else' (line, Name names) ((_, Lexer.Separator '.') : (_, Lexer.Name name) : tokens) = Right ((line, Name $ names ++ [name]), tokens)
        else' obj ((line, Lexer.Separator '.') : (_, Lexer.Name name) : tokens) = Right ((line, Access obj name), tokens)
        else' obj ((line, Lexer.Separator c) : _) = err line $ "Expected field or builtin name after " ++ [c]
        parseValues = parseMany parseValue $ Lexer.Separator ']'

parseValue :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Value), [(Integer, Lexer.Token)])
parseValue tokens = parseCall tokens >>= uncurry (tailRec2M if' id id else')
  where if' first tokens = Right (case tokens of (_, Lexer.Operator _) : _ -> False; _ -> True)
        else' first ((line, Lexer.Operator op) : tokens) = fmap2 (\second -> (line, Operation op [first, second])) id $ parseCall tokens

parseIf :: Integer -> [((Integer, Value), [(Integer, Statement)])] -> [(Integer, Lexer.Token)] -> Fallible ((Integer, Statement), [(Integer, Lexer.Token)])
parseIf line chain tokens = do
  (pair, tokens') <- follow parseValue parseBlock tokens
  let chain' = pair : chain
  case tokens' of
    (_, Lexer.Name "else") : (_, Lexer.Name "if") : tokens'' -> parseIf line chain' tokens''
    (_, Lexer.Name "else") : tokens'' -> do
      (elseBlock, tokens''') <- parseBlock tokens''
      Right ((line, IfChain (reverse chain') $ elseBlock), tokens''')
    _ -> Right ((line, IfChain (reverse chain') []), tokens')

parseStatement :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Statement), [(Integer, Lexer.Token)])
parseStatement ((line, Lexer.Name "if") : tokens) = parseIf line [] tokens
parseStatement ((line, Lexer.Name "while") : tokens) = do
  ((condition, block), tokens') <- follow parseValue parseBlock tokens
  Right ((line, While condition block), tokens')

parseStatement ((line, Lexer.Name name) : (_, Lexer.Operator "=") : tokens) = do
  (value, tokens') <- parseValue tokens
  Right ((line, Assignment name value), tokens')

parseStatement tokens@((line, _):_) = fmap2 ((,) line . Value) id $ parseValue tokens

parseBlock :: [(Integer, Lexer.Token)] -> Fallible ([(Integer, Statement)], [(Integer, Lexer.Token)])
parseBlock tokens = expect (Lexer.Separator '{') tokens >>= parseMany parseStatement (Lexer.Separator '}')

parseDeclaration :: [(Integer, Lexer.Token)] -> Fallible ((Integer, Bool, Declaration), [(Integer, Lexer.Token)])
parseDeclaration tokens@((line, Lexer.Name name) : _) = do
  let (tokens', export) = if name == "export" then (tail tokens, True) else (tokens, False)
  fmap2 ((,,) line export) id $ case tokens' of
    (_, Lexer.Name "import") : (_, Lexer.Name name) : (_, Lexer.LiteralText path) : tokens'' -> Right (Import name path, tokens'')
    (_, Lexer.Name "import") : (_, Lexer.Name "php") : (_, Lexer.Name name) : (_, Lexer.LiteralText path) : tokens'' -> do
      tokens''' <- expect (Lexer.Separator '{') tokens''
      (imports, tokens'''') <- parseFields Map.empty tokens'''
      Right (Php name path imports, tokens'''')
    (_, Lexer.Name name) : tokens'' -> do
      tokens''' <- expect (Lexer.Separator '=') tokens''
      (value, tokens'''') <- parseValue tokens'''
      Right (Declaration name value, tokens''')
    (_, token) : _ -> err line $ "Expected a name to export but got " ++ show token

parseDeclaration ((line, token) : _) = err line $ "Expected a name of declared function but got " ++ show token

parse :: [(Integer, Lexer.Token)] -> Fallible File
parse = tailRecM (Right . null . snd) (Right . File . reverse . fst) else' . (,) []
  where else' (result, tokens) = fmap2 (: result) id $ parseDeclaration tokens
