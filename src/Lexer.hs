module Lexer (Token (..), Operator (..), Keyword (..), tokenize) where

import Data.Char (ord, isDigit, isLetter, isSpace)
import Data.Ratio ((%))
import qualified Data.Map as Map
import Algorithms (distinctSort)

between :: (Ord t) => t -> t -> t -> Bool
between min max value = value <= max && value >= min

clamp :: (Ord t) => t -> t -> t -> t
clamp min max value = if value > max then max else if value < min then min else value

isDigitIn :: Integer -> Char -> Bool
isDigitIn radix c = between 0 r (ord c - ord '0') || between 0 (r - 10) (ord c - ord 'A') || between 0 (r - 10) (ord c - ord 'a')
    where r = fromIntegral (clamp 1 36 radix - 1) :: Int

digitToInteger :: Char -> Integer
digitToInteger c = ifBetween '0' '9' 0 $ ifBetween 'A' 'Z' 10 $ ifBetween 'a' 'z' 10 $ error $ "Character '" ++ [c] ++ "' is not a digit"
    where ifBetween min max add elsVal = if between min max c then toInteger $ ord c - ord min + add else elsVal

data Operator = Plus | Minus | Multiply | Divide | IntDivide | Modulo | And | Or | Xor | RaiseToThePowerOf | Assign
    | Equal | NotEqual | GreaterThan | LowerThan | GreaterThanOrEqualTo | LowerThanOrEqualTo | Apply | ShiftLeft
    | ShiftRight | ParenthesisOpen | ParenthesisClose | BracketOpen | BracketClose | Colon deriving (Show, Eq)

data Keyword = If | Elif | Else | End | While | Fun deriving (Show, Eq)

data LexToken = LexEmpty
    | LexComment
    | LexInteger Integer Integer
    | LexDecimal Integer Integer Integer
    | LexString String
    | LexName String
    | LexOperator Operator String
    | LexError String
    deriving (Show, Eq)

data Token = LiteralInteger Integer
    | LiteralDecimal Rational
    | LiteralString String
    | Identifier String
    | Keyword Keyword
    | Operator Operator
    | Error String
    deriving (Show, Eq)

operatorMap = Map.fromList [
    ("+", Plus), ("-", Minus), ("*", Multiply), ("/", Divide), ("//", IntDivide), ("%", Modulo),
    ("&", And), ("|", Or), ("^", Xor), ("**", RaiseToThePowerOf), ("->", Assign), ("=", Equal),
    ("~", NotEqual), ("<", LowerThan), (">", GreaterThan), (">=", GreaterThanOrEqualTo),
    ("<=", LowerThanOrEqualTo), ("'", Apply), ("=>", GreaterThanOrEqualTo), ("=<", LowerThanOrEqualTo),
    ("<<", ShiftLeft), (">>", ShiftRight), ("(", ParenthesisOpen), (")", ParenthesisClose),
    ("[", BracketOpen), ("]", BracketClose), (":", Colon)]

operators = Map.keys operatorMap

lookupOperator :: String -> LexToken
lookupOperator s = case Map.lookup s operatorMap of
    Just op -> LexOperator op s
    Nothing -> LexError s

extendOperator :: String -> Char -> (Char -> [LexToken]) -> [LexToken]
extendOperator current char fromChar = case lookupOperator $ current ++ [char] of
    LexOperator op s -> [LexOperator op s]
    _ -> fromChar char ++ [lookupOperator current]

lookupKeyword :: (Keyword -> Token) -> (String -> Token) -> String -> Token
lookupKeyword keyword2Token string2Token word = case Map.lookup word keywordMap of
    Just kw -> keyword2Token kw
    Nothing -> string2Token word
    where keywordMap = Map.fromList [("if", If), ("elif", Elif), ("else", Else), ("end", End), ("while", While), ("fun", Fun)]

nonEmpty :: LexToken -> [LexToken]
nonEmpty LexEmpty = []
nonEmpty token = [token]

processToken :: LexToken -> Char -> [LexToken]
processToken token char = case token of
    LexString ('^' : s) -> [LexString $ char : s]
    LexString s | char == '"' -> [LexEmpty, LexString s]
    LexString s -> [LexString $ char : s]
    LexInteger radix n | isDigitIn radix char -> [LexInteger radix $ n * radix + digitToInteger char]
    LexInteger radix n | char == '.' -> [LexDecimal radix n 0]
    LexInteger 10 n | (n /= 10) && (char == 'r') -> [LexInteger n 0]
    LexDecimal radix n exp | isDigitIn radix char -> [LexDecimal radix (radix * n + digitToInteger char) $ exp + 1]
    LexName name | isLetter char || isDigit char || char == '_' || char == '@' -> [LexName $ char : name]
    LexComment | char == '\n' -> [LexEmpty]
    LexComment -> [LexComment]
    LexOperator op s -> extendOperator s char $ processToken LexEmpty
    _ | isDigit char -> LexInteger 10 (digitToInteger char) : nonEmpty token
    _ | isLetter char || char == '_' || char == '@' -> LexName [char] : nonEmpty token
    _ | isSpace char -> LexEmpty : nonEmpty token
    _ | char == '"' -> LexString "" : nonEmpty token
    _ | char == ';' -> LexComment : nonEmpty token
    _ -> lookupOperator [char] : nonEmpty token

addToken :: [LexToken] -> Char -> [LexToken]
addToken (token : tokens) char = processToken token char ++ tokens

parseTokens :: String -> [LexToken]
parseTokens = foldl addToken [LexEmpty]

convertToken :: LexToken -> [Token]
convertToken lexToken = case lexToken of
    LexEmpty -> []
    LexComment -> []
    LexInteger _ n -> [LiteralInteger n]
    LexDecimal radix n exp -> [LiteralDecimal $ n % (radix ^ exp)]
    LexString string -> [LiteralString $ reverse string]
    LexName string -> [lookupKeyword Keyword Identifier $ reverse string]
    LexOperator op _ -> [Operator op]
    LexError string -> [Error string]

tokenize :: String -> [Token]
tokenize = concat . map convertToken . reverse . parseTokens
