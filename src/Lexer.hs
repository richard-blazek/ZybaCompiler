module Lexer (Token (..), Operator (..), Keyword (..), tokenize) where

import Data.Char (ord)
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

isLetter c = between 'A' 'Z' c || between 'a' 'z' c
isNameStart c = isLetter c || c == '@' || c == '_'
isNameFollowUp c = isNameStart c || isDigitIn 10 c
isSpace = (`elem` " \n\r\t,")

digitToInteger :: Char -> Integer
digitToInteger c = ifBetween '0' '9' 0 $ ifBetween 'A' 'Z' 10 $ ifBetween 'a' 'z' 10 $ error $ "Character '" ++ [c] ++ "' is not a digit"
    where ifBetween min max add elsVal = if between min max c then toInteger $ ord c - ord min + add else elsVal

data Operator = Plus | Minus | Multiply | Divide | IntDivide | Modulo | And | Or | Xor | RaiseToThePowerOf | Assign
    | Equal | NotEqual | GreaterThan | LowerThan | GreaterThanOrEqualTo | LowerThanOrEqualTo | ShiftLeft | ShiftRight
    | ParenthesisOpen | ParenthesisClose | BracketOpen | BracketClose deriving (Show, Eq)

data Keyword = If | Elif | Else | End | While | Fun deriving (Show, Eq)

data LexToken = LexEmpty
    | LexComment
    | LexInteger Integer Integer
    | LexDecimal Integer Integer Integer
    | LexString Bool String
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
    ("+", Plus), ("-", Minus), ("*", Multiply), ("/", Divide), (":", IntDivide), ("\\", Modulo),
    ("&", And), ("|", Or), ("'", Xor), ("^", RaiseToThePowerOf), ("->", Assign), ("=", Equal),
    ("~", NotEqual), ("<", LowerThan), (">", GreaterThan), (">=", GreaterThanOrEqualTo),
    ("<=", LowerThanOrEqualTo), ("=>", GreaterThanOrEqualTo), ("=<", LowerThanOrEqualTo),
    ("<<", ShiftLeft), (">>", ShiftRight), ("(", ParenthesisOpen), (")", ParenthesisClose),
    ("[", BracketOpen), ("]", BracketClose)]

operators = Map.keys operatorMap

lookupOperator :: String -> LexToken
lookupOperator s = case Map.lookup s operatorMap of
    Just op -> LexOperator op s
    Nothing -> LexError s

extendOperator :: String -> Char -> (Char -> [LexToken]) -> [LexToken]
extendOperator current char fromChar = case lookupOperator $ current ++ [char] of
    LexOperator op s -> [LexOperator op s]
    _ -> fromChar char ++ [lookupOperator current]

startToken :: Char -> [LexToken]
startToken char
    | isDigitIn 10 char = [LexInteger 10 $ digitToInteger char]
    | isNameStart char = [LexName [char]]
    | isSpace char = [LexEmpty]
    | char == '"' = [LexString False ""]
    | char == ';' = [LexComment]
    | otherwise = [lookupOperator [char]]

processToken :: [LexToken] -> Char -> [LexToken]
processToken tokens char = case tokens of
    LexString True s : rest | char == '"' -> LexString False ('"' : s) : rest
    LexString False s : rest | char == '"' -> LexString True s : rest
    LexString False s : rest -> LexString False (char : s) : rest
    LexInteger radix n : rest | isDigitIn radix char -> LexInteger radix (n * radix + digitToInteger char) : rest
    LexInteger radix n : rest | char == '.' -> LexDecimal radix n 0 : rest
    LexInteger 10 n : rest | (n /= 10) && (char == 'r') -> LexInteger n 0 : rest
    LexDecimal radix n exp : rest | isDigitIn radix char -> LexDecimal radix (radix * n + digitToInteger char) (exp + 1) : rest
    LexName name : rest | isNameFollowUp char -> LexName (char : name) : rest
    LexComment : rest | char == '\n' -> rest
    LexComment : rest -> LexComment : rest
    LexOperator op s : rest -> extendOperator s char startToken ++ rest
    LexEmpty : rest -> startToken char ++ rest
    token : rest -> startToken char ++ (token : rest)
    [] -> startToken char

parseTokensWith :: [LexToken] -> String -> [LexToken]
parseTokensWith tokens (char : string) = parseTokensWith (processToken tokens char) string
parseTokensWith tokens [] = tokens

lookupKeyword :: String -> Token
lookupKeyword word = case Map.lookup word keywordMap of
    Just kw -> Keyword kw
    Nothing -> Identifier word
    where keywordMap = Map.fromList [("if", If), ("elif", Elif), ("else", Else), ("end", End), ("while", While), ("fun", Fun)]

convertToken :: LexToken -> [Token]
convertToken lexToken = case lexToken of
    LexEmpty -> []
    LexComment -> []
    LexInteger _ n -> [LiteralInteger n]
    LexDecimal radix n exp -> [LiteralDecimal $ n % (radix ^ exp)]
    LexString _ string -> [LiteralString $ reverse string]
    LexName string -> [lookupKeyword $ reverse string]
    LexOperator op _ -> [Operator op]
    LexError string -> [Error string]

tokenize :: String -> [Token]
tokenize = concat . map convertToken . reverse . parseTokensWith []
