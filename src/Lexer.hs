module Lexer (Token (..), Operator (..), Keyword (..), tokenize) where

import Data.Char (ord)
import qualified Data.Map as Map

data Operator = Plus | Minus | Multiply | Divide | IntDivide | Modulo | And | Or | Xor | RaiseToThePowerOf | Assign
    | Equal | NotEqual | GreaterThan | LowerThan | GreaterThanOrEqualTo | LowerThanOrEqualTo | ShiftLeft | ShiftRight
    | ParenthesisOpen | ParenthesisClose | BracketOpen | BracketClose deriving (Show, Eq)

data Keyword = If | Elif | Else | End | While | Fun deriving (Show, Eq)

data Token = Empty
    | Comment
    | LiteralInteger Integer Integer
    | LiteralDecimal Integer Integer Integer
    | LiteralString String
    | Identifier String
    | Keyword Keyword String
    | Operator Operator String
    | Unknown String
    deriving (Show)

instance Eq Token where
    Empty == Empty = True
    Comment == Comment = True
    LiteralInteger _ v1 == LiteralInteger _ v2 = v1 == v2
    LiteralDecimal _ v1 e1 == LiteralDecimal _ v2 e2 = (v1, e1) == (v2, e2)
    LiteralString s1 == LiteralString s2 = s1 == s2
    Identifier s1 == Identifier s2 = s1 == s2
    Keyword k1 _ == Keyword k2 _ = k1 == k2
    Operator o1 _ == Operator o2 _ = o1 == o2
    Unknown e1 == Unknown e2 = e1 == e2
    _ == _ = False

between :: (Ord t) => t -> t -> t -> Bool
between min max value = value <= max && value >= min

parseDigit :: Char -> Integer
parseDigit c = inRange '0' '9' 0 $ inRange 'A' 'Z' 10 $ inRange 'a' 'z' 10 $ 36
    where inRange min max add elsVal = if between min max c then toInteger $ ord c - ord min + add else elsVal

isLetter c = between 'A' 'Z' c || between 'a' 'z' c
isNameStart c = isLetter c || c == '@' || c == '_'
isNameFollowUp c = isNameStart c || parseDigit c < 10
isSpace = (`elem` " \n\r\t")

keywords = Map.fromList [("if", If), ("elif", Elif), ("else", Else), ("end", End), ("while", While), ("fun", Fun)]
operators = Map.fromList [
    ("+", Plus), ("-", Minus), ("*", Multiply), ("/", Divide), (":", IntDivide), ("\\", Modulo),
    ("&", And), ("|", Or), ("'", Xor), ("^", RaiseToThePowerOf), ("->", Assign), ("=", Equal),
    ("~", NotEqual), ("<", LowerThan), (">", GreaterThan), (">=", GreaterThanOrEqualTo),
    ("<=", LowerThanOrEqualTo), ("<<", ShiftLeft), (">>", ShiftRight), ("(", ParenthesisOpen),
    (")", ParenthesisClose), ("[", BracketOpen), ("]", BracketClose)]

lookupKeyword :: String -> Token
lookupKeyword s = case Map.lookup s keywords of
    Just kw -> Keyword kw s
    Nothing -> Identifier s

lookupOperator :: String -> Token
lookupOperator s = case Map.lookup s operators of
    Just op -> Operator op s
    Nothing -> Unknown s

extendOperator :: String -> Char -> (Char -> [Token]) -> [Token]
extendOperator current char fromChar = case lookupOperator $ current ++ [char] of
    Unknown _ -> fromChar char ++ [lookupOperator current]
    operator -> [operator]

startToken :: Char -> [Token]
startToken char
    | char == '"' = [LiteralString ""]
    | char == ';' = [Comment]
    | isSpace char = [Empty]
    | parseDigit char < 10 = [LiteralInteger 10 $ parseDigit char]
    | isNameStart char = [lookupKeyword [char]]
    | otherwise = [lookupOperator [char]]

processToken :: [Token] -> Char -> [Token]
processToken tokens char = case tokens of
    Empty : LiteralString s : rest | char == '"' -> LiteralString ('"' : reverse s) : rest
    LiteralString s : rest | char == '"' -> Empty : LiteralString (reverse s) : rest
    LiteralString s : rest -> LiteralString (char : s) : rest
    LiteralInteger radix n : rest | parseDigit char < radix -> LiteralInteger radix (n * radix + parseDigit char) : rest
    LiteralInteger radix n : rest | char == '.' -> LiteralDecimal radix n 0 : rest
    LiteralInteger 10 n : rest | (n /= 10) && (char == 'r') -> LiteralInteger n 0 : rest
    LiteralDecimal radix n exp : rest | parseDigit char < radix -> LiteralDecimal radix (radix * n + parseDigit char) (exp + 1) : rest
    Identifier name : rest | isNameFollowUp char -> lookupKeyword (name ++ [char]) : rest
    Comment : rest | char == '\n' -> Empty : rest
    Comment : rest -> Comment : rest
    Operator _ s : rest -> extendOperator s char startToken ++ rest
    Empty : rest -> startToken char ++ rest
    tokens -> startToken char ++ tokens

tokenize :: String -> [Token]
tokenize = reverse . foldl processToken []
