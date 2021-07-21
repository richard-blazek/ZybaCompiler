module Lexer (Token (..), Operator (..), Keyword (..), tokenize) where

import Data.Char (chr, ord, toUpper)
import qualified Data.Map as Map
import Algorithms (distinctSort)

data Operator = Plus | Minus | Multiply | Divide | IntDivide | Modulo | And | Or | Xor | RaiseToThePowerOf | Assign
    | Equal | NotEqual | GreaterThan | LowerThan | GreaterThanOrEqualTo | LowerThanOrEqualTo | Apply | ShiftLeft
    | ShiftRight deriving (Show, Eq)
data Keyword = If | Elif | Else | End | While | Fun deriving (Show, Eq)

data Token = Empty
    | Comment
    | Identifier String
    | LiteralInteger Integer Integer
    | LiteralDecimal Integer Integer Integer
    | LiteralString String
    | Operator Operator
    | Keyword Keyword
    | ParenthesisOpen
    | ParenthesisClose
    | BracketOpen
    | BracketClose
    | Colon
    | Error String
    | UnknownOperator String
    deriving (Show, Eq)

operatorList = [
    ("+", Plus), ("-", Minus), ("*", Multiply), ("/", Divide), ("//", IntDivide), ("%", Modulo),
    ("&", And), ("|", Or), ("^", Xor), ("**", RaiseToThePowerOf), ("->", Assign), ("=", Equal),
    ("~", NotEqual), ("<", LowerThan), (">", GreaterThan), (">=", GreaterThanOrEqualTo),
    ("<=", LowerThanOrEqualTo), ("'", Apply), ("=>", GreaterThanOrEqualTo), ("=<", LowerThanOrEqualTo),
    ("<<", ShiftLeft), (">>", ShiftRight)]

operatorMap = Map.fromList operatorList
operatorChars = (distinctSort . concat) (map fst operatorList)

lookupOperator :: String -> Token
lookupOperator s = case Map.lookup s operatorMap of
    Just op -> Operator op
    Nothing -> Error s

between :: (Ord t) => t -> t -> t -> Bool
between min max value = value <= max && value >= min

isDigit = between '0' '9'
isLetter c = between 'A' 'Z' c || between 'a' 'z' c || c == '_'
isWhite = (`elem` " \n\r\t")
isOperatorChar = (`elem` operatorChars)

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
keywordOrIdentifier "fun" = Keyword Fun
keywordOrIdentifier name = Identifier name

processToken :: Token -> Char -> [Token]
processToken token char = case token of
    LiteralString ('^' : s) -> [LiteralString (char : s)]
    LiteralString s | char == '"' -> [Empty, LiteralString (reverse s)]
    LiteralString s -> [LiteralString (char:s)]
    LiteralInteger radix n | isDigitIn radix char -> [LiteralInteger radix (n * radix + digitToInteger char)]
    LiteralInteger radix n | char == '.' -> [LiteralDecimal radix n 0]
    LiteralInteger 10 n | (n /= 10) && (char == 'r') -> [LiteralInteger n 0]
    LiteralDecimal radix n exp | isDigitIn radix char -> [LiteralDecimal radix (radix * n + digitToInteger char) (exp + 1)]
    Identifier name | isLetter char || isDigit char -> [keywordOrIdentifier (name ++ [char])]
    Comment | char == '\n' -> [Empty]
    Comment -> [Comment]
    UnknownOperator s | isOperatorChar char -> [UnknownOperator (char : s)]
    UnknownOperator s -> processToken Empty char ++ [lookupOperator (reverse s)]
    _ -> case char of
        '"' -> nonEmpty (LiteralString "") token
        '(' -> nonEmpty ParenthesisOpen token
        ')' -> nonEmpty ParenthesisClose token
        '[' -> nonEmpty BracketOpen token
        ']' -> nonEmpty BracketClose token
        ':' -> nonEmpty Colon token
        ';' -> nonEmpty Comment token
        _ | isOperatorChar char -> nonEmpty (UnknownOperator [char]) token
        _ | isDigit char -> nonEmpty (LiteralInteger 10 (digitToInteger char)) token
        _ | isLetter char -> nonEmpty (keywordOrIdentifier [char]) token
        _ | isWhite char -> Empty : (nonEmpty Empty token)
        _ -> [Error [char]]

addToken :: [Token] -> Char -> [Token]
addToken (token : tokens) char = processToken token char ++ tokens

tokenize :: String -> [Token]
tokenize input = reverse (foldl addToken [Empty] ('\n' : input))