module Lexer (Token (..), OperatorType (..), KeywordType (..), tokenize, processToken) where

import Data.Char (chr, ord, toUpper)

data OperatorType = Plus | Minus | Multiply | Divide | IntDivide | Modulo | And | Or | Xor | RaiseToThePowerOf | Assign
        | Equal | NotEqual | GreaterThan | LowerThan | GreatherThanOrEqualTo | LowerThanOrEqualTo | Apply deriving (Show, Eq)
data KeywordType = If | Elif | Else | End | While deriving (Show, Eq)

data Token = Empty
        | Comment Int
        | Identifier String
        | LiteralInteger Integer Integer
        | LiteralFloat Integer Integer Integer
        | LiteralString String
        | Operator OperatorType
        | Keyword KeywordType
        | ParenthesisOpen
        | ParenthesisClose
        | BracketOpen
        | BracketClose
        | Error Char
        deriving (Show, Eq)

between :: (Ord t) => t -> t -> t -> Bool
between min max value = value <= max && value >= min

isDigit = between '0' '9'
isLetter c = between 'A' 'Z' c || between 'a' 'z' c || c == '_'
isWhite = (`elem` [' ', '\n', '\r', '\t'])

isDigitIn :: Integer -> Char -> Bool
isDigitIn radix c
        | intRadix <= 10 = between '0' (chr (ord '0' + intRadix - 1)) c
        | otherwise = isDigit c || between 'A' (chr (ord 'A' + intRadix - 11)) c
        where intRadix = fromIntegral radix :: Int

digitToInteger :: Char -> Integer
digitToInteger c
        | isDigit d = toInteger (ord d - ord '0')
        | d >= 'A' && d <= 'Z' = toInteger (ord d - ord 'A' + 10)
        | otherwise = error ("'" ++ [c] ++ "' character is not a digit")
        where d = toUpper c

nonEmpty :: Token -> Token -> [Token]
nonEmpty Empty Empty = []
nonEmpty Empty token = [token]
nonEmpty token Empty = [token]
nonEmpty t1 t2 = [t1, t2]

keywordOrIdentifier :: String -> Token
keywordOrIdentifier "if" = Keyword If
keywordOrIdentifier "elif" = Keyword Elif
keywordOrIdentifier "else" = Keyword Else
keywordOrIdentifier "end" = Keyword End
keywordOrIdentifier "while" = Keyword While
keywordOrIdentifier name = Identifier name

processToken :: Token -> Char -> [Token]
processToken token char = case token of
        LiteralString ('^':s) -> [LiteralString (char:s)]
        LiteralString s | char == '"' -> [Empty, LiteralString (reverse s)]
        LiteralString s -> [LiteralString (char:s)]
        LiteralInteger radix n | isDigitIn radix char -> [LiteralInteger radix (n * radix + digitToInteger char)]
        LiteralInteger radix n | char == '.' -> [LiteralFloat radix n 0]
        LiteralInteger 10 n | (n /= 10) && (char == 'r') -> [LiteralInteger n 0]
        LiteralFloat radix n exp | isDigitIn radix char -> [LiteralFloat radix (n * radix + digitToInteger char) (exp + 1)]
        Identifier name | isLetter char -> [keywordOrIdentifier (name ++ [char])]
        Comment 1 | char == '}' -> [Empty]
        Comment level | char == '}' -> [Comment (level - 1)]
        Comment level | char == '{' -> [Comment (level + 1)]
        Comment level -> [Comment level]
        Operator Multiply | char == '*' -> [Operator RaiseToThePowerOf]
        Operator Minus | char == '>' -> [Operator Assign]
        Operator GreaterThan | char == '=' -> [Operator GreatherThanOrEqualTo]
        Operator LowerThan | char == '=' -> [Operator LowerThanOrEqualTo]
        Operator Divide | char == '/' -> [Operator IntDivide]
        _ -> case char of
                '+' -> nonEmpty (Operator Plus) token
                '-' -> nonEmpty (Operator Minus) token
                '*' -> nonEmpty (Operator Multiply) token
                '/' -> nonEmpty (Operator Divide) token
                '%' -> nonEmpty (Operator Modulo) token
                '&' -> nonEmpty (Operator And) token
                '|' -> nonEmpty (Operator Or) token
                '^' -> nonEmpty (Operator Xor) token
                '~' -> nonEmpty (Operator NotEqual) token
                '<' -> nonEmpty (Operator GreaterThan) token
                '>' -> nonEmpty (Operator LowerThan) token
                '=' -> nonEmpty (Operator Equal) token
                ':' -> nonEmpty (Operator Apply) token
                '"' -> nonEmpty (LiteralString "") token
                ')' -> nonEmpty ParenthesisClose token
                '[' -> nonEmpty BracketOpen token
                ']' -> nonEmpty BracketClose token
                '{' -> nonEmpty (Comment 1) token
                _ | isDigit char -> nonEmpty (LiteralInteger 10 (digitToInteger char)) token
                _ | isLetter char -> nonEmpty (keywordOrIdentifier [char]) token
                _ | isWhite char -> Empty : (nonEmpty Empty token)
                _ -> [Error char]


addToken :: [Token] -> Char -> [Token]
addToken (token:tokens) char = processToken token char ++ tokens

tokenize :: String -> [Token]
tokenize input = reverse (foldl addToken [Empty] input)