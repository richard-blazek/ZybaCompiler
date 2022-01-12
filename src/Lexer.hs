module Lexer (Token (..), tokenize) where

import Data.Char (ord)

data Token
  = Comment
  | Empty
  | LiteralReal Integer Integer Integer
  | LiteralInt Integer Integer
  | LiteralText String
  | LiteralBool Bool
  | Operator String
  | Separator Char
  | Word String deriving (Show, Eq)

between :: Ord t => t -> t -> t -> Bool
between min max value = value <= max && value >= min

parseDigit :: Char -> Integer
parseDigit c = inRange '0' '9' 0 $ inRange 'A' 'Z' 10 $ inRange 'a' 'z' 10 36
  where inRange min max add elsVal = if between min max c then toInteger $ ord c - ord min + add else elsVal

isDigit radix c = let n = parseDigit c in n < 36 && n < radix
isAlpha c = between 'A' 'Z' c || between 'a' 'z' c || c == '_'
isAlnum c = isAlpha c || isDigit 10 c
isOperator = (`elem` "+-*/%&|~^<>=!")
isSeparator = (`elem` "()[]{}:.")

startToken :: Char -> Token
startToken '"' = LiteralText ""
startToken ';' = Comment
startToken char
  | isDigit 10 char = LiteralInt 10 $ parseDigit char
  | isOperator char = Operator [char]
  | isSeparator char = Separator char
  | isAlpha char = Word [char]
  | otherwise = Empty

buildToken :: [(Integer, Token)] -> Char -> [(Integer, Token)]
buildToken tokens char = case tokens of
  (line, Comment) : rest | char == '\n' -> (line + inc, Empty) : rest
  (line, Comment) : rest -> (line + inc, Comment) : rest
  (_, Empty) : (line, LiteralText s) : rest | char == '"' -> (line + inc, LiteralText $ '"' : reverse s) : rest
  (line, LiteralText s) : rest | char == '"' -> (line + inc, Empty) : (line, LiteralText $ reverse s) : rest
  (line, LiteralText s) : rest -> (line + inc, LiteralText $ char : s) : rest
  (line, LiteralInt radix n) : rest | isDigit radix char -> (line + inc, LiteralInt radix $ n * radix + parseDigit char) : rest
  (line, LiteralInt radix n) : rest | char == '.' -> (line + inc, LiteralReal radix n 0) : rest
  (line, LiteralInt radix n) : rest | char == 'b' && between 0 1 n -> (line + inc, LiteralBool (n == 1)) : rest
  (line, LiteralInt 10 n) : rest | (n /= 10) && (char == 'r') -> (line + inc, LiteralInt n 0) : rest
  (line, LiteralReal radix n exp) : rest | isDigit radix char -> (line + inc, LiteralReal radix (radix * n + parseDigit char) $ exp + 1) : rest
  (line, Word name) : rest | isAlnum char -> (line + inc, Word $ name ++ [char]) : rest
  (line, Operator s) : rest | isOperator char -> (line + inc, Operator $ s ++ [char]) : rest
  (line, Empty) : rest | not (null rest) -> (line + inc, startToken char) : rest
  token@(line, _) : rest -> (line + inc, startToken char) : token : rest
  where inc = if char == '\n' then 1 else 0

tokenize :: String -> [(Integer, Token)]
tokenize = tail . reverse . dropWhile ((`elem` [Empty, Comment]) . snd) . foldl buildToken [(0, Empty)]
