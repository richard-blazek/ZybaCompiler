module Lexer (Token (Empty, Identifier, LiteralInteger, LiteralFloat, LiteralString, Operator, Keyword, ParenthesisOpen, ParenthesisClose, BraceOpen, BraceClose),
        OperatorType (PlusOperator, MinusOperator, MultiplyOperator, DivideOperator),
        KeywordType (IfKeyword, ElseKeyword, WhileKeyword),
        tokenize) where

import Data.Char (digitToInt)

data OperatorType = PlusOperator | MinusOperator | MultiplyOperator | DivideOperator deriving (Show, Eq)
data KeywordType = IfKeyword | ElseKeyword | WhileKeyword deriving (Show, Eq)

data Token = Empty
        | Identifier String
        | LiteralInteger Integer
        | LiteralFloat Integer Integer
        | LiteralString String
        | Operator OperatorType
        | Keyword KeywordType
        | ParenthesisOpen
        | ParenthesisClose
        | BraceOpen
        | BraceClose deriving (Show, Eq)

isDigit c = c >= '0' && c <= '9'
isLetter c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
digitToInteger d = toInteger (digitToInt d)

nonEmpty :: Token -> Token -> [Token]
nonEmpty Empty Empty = []
nonEmpty Empty token = [token]
nonEmpty token Empty = [token]
nonEmpty t1 t2 = [t1, t2]

keywordOrIdentifier :: String -> Token
keywordOrIdentifier "if" = Keyword IfKeyword
keywordOrIdentifier "else" = Keyword ElseKeyword
keywordOrIdentifier "while" = Keyword WhileKeyword
keywordOrIdentifier name = Identifier name

processToken :: Token -> Char -> [Token]
processToken token '"' = nonEmpty (LiteralString "") token
processToken token '+' = nonEmpty (Operator PlusOperator) token
processToken token '-' = nonEmpty (Operator MinusOperator) token
processToken token '*' = nonEmpty (Operator MultiplyOperator) token
processToken token '/' = nonEmpty (Operator DivideOperator) token
processToken token '{' = nonEmpty BraceOpen token
processToken token '}' = nonEmpty BraceClose token
processToken token '(' = nonEmpty ParenthesisOpen token
processToken token ')' = nonEmpty ParenthesisClose token

processToken token char = case token of
        LiteralString s | char == '"' -> [Empty, LiteralString (reverse s)]
        LiteralString s -> [LiteralString (char:s)]
        LiteralInteger num | isDigit char -> [LiteralInteger (num * 10 + digitToInteger char)]
        LiteralInteger num | char == '.' -> [LiteralFloat num 0]
        LiteralFloat num exp | isDigit char -> [LiteralFloat (num * 10 + digitToInteger char) (exp + 1)]
        Identifier name | isLetter char -> [keywordOrIdentifier (name ++ [char])]
        _ | isDigit char -> nonEmpty (LiteralInteger (digitToInteger char)) token
        _ | isLetter char -> nonEmpty (keywordOrIdentifier [char]) token
        Empty -> [Empty]
        _ -> [Empty, token]

addToken :: [Token] -> Char -> [Token]
addToken (token:tokens) char = processToken token char ++ tokens

tokenize :: String -> [Token]
tokenize input = reverse (foldl addToken [Empty] input)