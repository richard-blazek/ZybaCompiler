module Codegen (generate) where

import qualified Data.Set as Set
import Scope (Type (..), primitives)
import Semantics (Value, ValueData (..), Statement (..))
import Errors (Fallible, failure)
import Functions (join)

capturesOfBlock :: Set.Set String -> [Statement] -> Set.Set String
capturesOfBlock skip [] = Set.empty
capturesOfBlock skip (Expression value : rest) = capturesOfValue skip value
capturesOfBlock skip (Initialization name value : rest) = Set.union (capturesOfValue skip value) $ capturesOfBlock (Set.insert name skip) rest
capturesOfBlock skip (Assignment _ value : rest) = Set.union (capturesOfValue skip value) $ capturesOfBlock skip rest
capturesOfBlock skip (While condition block : rest) = Set.unions [capturesOfValue skip condition, capturesOfBlock skip block, capturesOfBlock skip rest]
capturesOfBlock skip (IfChain chains else' : rest) = Set.unions [Set.unions $ map (\(cond, block) -> Set.union (capturesOfValue skip cond) $ capturesOfBlock skip block) chains, capturesOfBlock skip else', capturesOfBlock skip rest]

capturesOfValue :: Set.Set String -> Value -> Set.Set String
capturesOfValue skip (_, Variable name) = if Set.member name skip then Set.empty else Set.singleton name
capturesOfValue skip (_, Lambda args body) = capturesOfBlock (Set.union skip $ Set.fromList $ map fst args) body
capturesOfValue skip (_, Call fun args) = Set.unions $ map (capturesOfValue skip) $ fun : args
capturesOfValue skip _ = Set.empty

stringifyCall :: Value -> [String] -> String
stringifyCall (Projection [Int, Int] Int, Variable "+") [a, b] = "((int)(" ++ a ++ "+" ++ b ++ "))"
stringifyCall (Projection [String, String] String, Variable "+") [a, b] = "(" ++ a ++ "." ++ b ++ ")"
stringifyCall (_, Variable "+") [a, b] = "(" ++ a ++ "+" ++ b ++ ")"
stringifyCall (Projection [Int, Int] Int, Variable "-") [a, b] = "((int)(" ++ a ++ "-" ++ b ++ "))"
stringifyCall (_, Variable "-") [a, b] = "(" ++ a ++ "-" ++ b ++ ")"
stringifyCall (Projection [Int, Int] Int, Variable "*") [a, b] = "((int)(" ++ a ++ "*" ++ b ++ "))"
stringifyCall (_, Variable "*") [a, b] = "(" ++ a ++ "*" ++ b ++ ")"
stringifyCall (_, Variable "/") [a, b] = "(" ++ a ++ "/(float)" ++ b ++ ")"
stringifyCall (_, Variable "//") [a, b] = "((int)(" ++ a ++ "/" ++ b ++ "))"
stringifyCall (Projection [Int, Int] Int, Variable "%") [a, b] = "(" ++ a ++ "%" ++ b ++ ")"
stringifyCall (_, Variable "%") [a, b] = "fmod(" ++ a ++ "," ++ b ++ ")"
stringifyCall (Projection [Int, Int] Int, Variable "&") [a, b] = "(" ++ a ++ "&" ++ b ++ ")"
stringifyCall (_, Variable "&") [a, b] = "(" ++ a ++ "&&" ++ b ++ ")"
stringifyCall (Projection [Int, Int] Int, Variable "|") [a, b] = "(" ++ a ++ "||" ++ b ++ ")"
stringifyCall (_, Variable "|") [a, b] = "(" ++ a ++ "||" ++ b ++ ")"
stringifyCall (Projection [Bool, Bool] Bool, Variable "^") [a, b] = "(" ++ a ++ "!==" ++ b ++ ")"
stringifyCall (_, Variable "^") [a, b] = "(" ++ a ++ "^" ++ b ++ ")"
stringifyCall (_, Variable "==") [a, b] = "(" ++ a ++ "===" ++ b ++ ")"
stringifyCall (_, Variable "!=") [a, b] = "(" ++ a ++ "!==" ++ b ++ ")"
stringifyCall (_, Variable "<") [a, b] = "(" ++ a ++ "<" ++ b ++ ")"
stringifyCall (_, Variable ">") [a, b] = "(" ++ a ++ ">" ++ b ++ ")"
stringifyCall (_, Variable "<=") [a, b] = "(" ++ a ++ "<=" ++ b ++ ")"
stringifyCall (_, Variable ">=") [a, b] = "(" ++ a ++ ">=" ++ b ++ ")"
stringifyCall (Projection [Int, Int] Int, Variable "**") [a, b] = "((int)(" ++ a ++ "**" ++ b ++ "))"
stringifyCall (_, Variable "**") [a, b] = "(" ++ a ++ "**" ++ b ++ ")"
stringifyCall (Projection [Int] Int, Variable "not") [a] = "(~" ++ a ++ ")"
stringifyCall (_, Variable "not") [a] = "(!" ++ a ++ ")"
stringifyCall (_, Variable "Int") [a] = "((int)" ++ a ++ ")"
stringifyCall (Projection [String] Bool, Variable "Bool") [a] = "(" ++ a ++ "!==\"\")"
stringifyCall (_, Variable "Bool") [a] = "((bool)" ++ a ++ ")"
stringifyCall (_, Variable "Float") [a] = "((float)" ++ a ++ ")"
stringifyCall (_, Variable "String") [a] = "((string)" ++ a ++ ")"
stringifyCall fun args = "(" ++ (stringifyValue fun) ++ "(" ++ (join "," args) ++ "))"

stringifyStatement :: Statement -> String
stringifyStatement (Expression value) = stringifyValue value ++ ";"
stringifyStatement (Initialization name value) = "$" ++ name ++ "=" ++ stringifyValue value ++ ";"
stringifyStatement (Assignment name value) = "$" ++ name ++ "=" ++ stringifyValue value ++ ";"
stringifyStatement (While condition block) = "while(" ++ stringifyValue condition ++ ")" ++ stringifyBlock False block
stringifyStatement (IfChain chain else') = join "else " (map (\(cond, block) -> "if(" ++ stringifyValue cond ++ ")" ++ stringifyBlock False block) chain) ++ "else" ++ stringifyBlock False else'

stringifyBlock :: Bool -> [Statement] -> String
stringifyBlock _ [] = ""
stringifyBlock True [statement@(Expression (type', _))] | type' /= Void = "return " ++ stringifyStatement statement
stringifyBlock return (statement : statements) = stringifyStatement statement ++ stringifyBlock return statements

stringifyValue :: Value -> String
stringifyValue (_, LiteralInt i) = show i
stringifyValue (_, LiteralFloat f) = show f
stringifyValue (_, LiteralString s) = show s
stringifyValue (_, Variable name) = '$' : name
stringifyValue (_, Call fun args) = stringifyCall fun $ map stringifyValue args
stringifyValue (Projection _ returnType', Lambda args block) = header ++ "{" ++ stringifyBlock (returnType' /= Void) block ++ "})"
  where argNames = map fst args
        captures = join "," $ Set.map ("&$" ++) $ Set.difference (capturesOfBlock (Set.fromList argNames) block) primitives
        header = "(function(" ++ join "," (map ('$':) argNames) ++ ")" ++ (if null captures then "" else "use(" ++ captures ++ ")")

stringifyDeclaration :: String -> Value -> String
stringifyDeclaration name value = "$" ++ name ++ "=" ++ (stringifyValue value) ++ ";"

generate :: [(String, Value)] -> String
generate globals = concat $ map (uncurry stringifyDeclaration) globals