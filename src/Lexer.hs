module Lexer (Lexeme (..), Token, tokenize) where

import Data.Char (ord)

data Lexeme
    = Comment
    | Empty
    | LiteralRational Integer Integer Integer
    | LiteralInteger Integer Integer
    | LiteralString String
    | Operator String
    | Separator Char
    | Word String
    deriving (Show, Read, Eq)

type Token = (Integer, Lexeme)

between :: (Ord t) => t -> t -> t -> Bool
between min max value = value <= max && value >= min

parseDigit :: Char -> Integer
parseDigit c = inRange '0' '9' 0 $ inRange 'A' 'Z' 10 $ inRange 'a' 'z' 10 36
    where inRange min max add elsVal = if between min max c then toInteger $ ord c - ord min + add else elsVal

isDigit = between '0' '9'
isAlpha c = between 'A' 'Z' c || between 'a' 'z' c || c == '_'
isAlnum c = isAlpha c || isDigit c
isOperator = (`elem` "+-*/:\\&|~^<>=!")
isSeparator = (`elem` "()[]")

startLexeme :: Char -> Lexeme
startLexeme char
    | char == '"' = LiteralString ""
    | char == ';' = Comment
    | isDigit char = LiteralInteger 10 $ parseDigit char
    | isOperator char = Operator [char]
    | isSeparator char = Separator char
    | isAlpha char = Word [char]
    | otherwise = Empty

buildToken :: [Token] -> Char -> [Token]
buildToken tokens char = case tokens of
    (line, Comment) : rest | char == '\n' -> (line, Empty) : rest
    (line, Comment) : rest -> (line, Comment) : rest
    (_, Empty) : (line, LiteralString s) : rest | char == '"' -> (line, LiteralString $ '"' : reverse s) : rest
    (line, LiteralString s) : rest | char == '"' -> (line, Empty) : (line, LiteralString $ reverse s) : rest
    (line, LiteralString s) : rest -> (line, LiteralString $ char : s) : rest
    (line, LiteralInteger radix n) : rest | parseDigit char < radix -> (line, LiteralInteger radix $ n * radix + parseDigit char) : rest
    (line, LiteralInteger radix n) : rest | char == '.' -> (line, LiteralRational radix n 0) : rest
    (line, LiteralInteger 10 n) : rest | (n /= 10) && (char == 'r') -> (line, LiteralInteger n 0) : rest
    (line, LiteralRational radix n exp) : rest | parseDigit char < radix -> (line, LiteralRational radix (radix * n + parseDigit char) $ exp + 1) : rest
    (line, Word name) : rest | isAlnum char -> (line, Word $ name ++ [char]) : rest
    (line, Operator s) : rest | isOperator char -> (line, Operator $ s ++ [char]) : rest
    (line, Empty) : rest | not (null rest) -> (line, startLexeme char) : rest
    token@(line, _) : rest -> (line, startLexeme char) : token : rest

tokenize :: String -> [Token]
tokenize = reverse . dropWhile (\x -> snd x `elem` [Empty, Comment]) . foldl buildToken [(0, Empty)]
