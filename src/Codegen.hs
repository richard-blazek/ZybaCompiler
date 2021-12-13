module Codegen (generate) where

import qualified Data.Set as Set
import Semantics (Value, ValueData (..), Statement, StatementData (..), Primitive (..))
import Errors (Fallible, failure)
import Functions (join)

capturesOfBlock :: Set.Set String -> [Statement] -> Set.Set String
capturesOfBlock skip ((_, _, Evaluation value) : rest) = capturesOfValue skip value
capturesOfBlock skip ((_, _, Initialization name value) : rest) = Set.union (capturesOfValue skip value) $ capturesOfBlock (Set.insert name skip) rest
capturesOfBlock skip ((_, _, Assignment _ value) : rest) = Set.union (capturesOfValue skip value) $ capturesOfBlock skip rest
capturesOfBlock skip ((_, _, While condition block) : rest) = Set.unions [capturesOfValue skip condition, capturesOfBlock skip [block], capturesOfBlock skip rest]
capturesOfBlock skip ((_, _, IfChain chains else') : rest) = Set.unions [Set.unions $ map (\(cond, block) -> Set.union (capturesOfValue skip cond) $ capturesOfBlock skip [block]) chains, capturesOfBlock skip [else'], capturesOfBlock skip rest]
capturesOfBlock skip ((_, _, Block statements) : rest) = Set.union (capturesOfBlock skip statements) $ capturesOfBlock skip rest

capturesOfValue :: Set.Set String -> Value -> Set.Set String
capturesOfValue skip (_, _, Variable name) = if Set.member name skip then Set.empty else Set.singleton name
capturesOfValue skip (_, _, Function args body) = let innerSkip = Set.union skip $ Set.fromList $ map fst args in capturesOfBlock innerSkip [body]
capturesOfValue skip (_, _, Operation fun args) = Set.unions $ map (capturesOfValue skip) $ fun : args
capturesOfValue skip _ = Set.empty

stringifyPrimitive :: Primitive -> [Value] -> String
stringifyPrimitive cmd args = case (cmd, map stringifyValue args) of
    (Add, [a, b]) -> "(" ++ a ++ "+" ++ b ++ ")"
    (Subtract, [a, b]) -> "(" ++ a ++ "-" ++ b ++ ")"
    (Multiply, [a, b]) -> "(" ++ a ++ "*" ++ b ++ ")"
    (Divide, [a, b]) -> "(" ++ a ++ "/" ++ b ++ ")"
    (Modulo, [a, b]) -> "(" ++ a ++ "%" ++ b ++ ")"
    (And, [a, b]) -> "(" ++ a ++ "&&" ++ b ++ ")"
    (Or, [a, b]) -> "(" ++ a ++ "||" ++ b ++ ")"
    (Xor, [a, b]) -> "(" ++ a ++ "^" ++ b ++ ")"
    (Equal, [a, b]) -> "(" ++ a ++ "==" ++ b ++ ")"
    (NotEqual, [a, b]) -> "(" ++ a ++ "!=" ++ b ++ ")"
    (LowerThan, [a, b]) -> "(" ++ a ++ "<" ++ b ++ ")"
    (GreaterThan, [a, b]) -> "(" ++ a ++ ">" ++ b ++ ")"
    (Power, [a, b]) -> "pow(" ++ a ++ "," ++ b ++ ")"
    (Not, [a]) ->  "(!" ++ a ++ ")"

stringifyStatement :: Bool -> Statement -> String
stringifyStatement return (_, _, Evaluation value) = (if return then "return " else "") ++ stringifyValue value ++ ";"
stringifyStatement _ (_, _, Initialization name value) = "$" ++ name ++ "=" ++ stringifyValue value ++ ";"
stringifyStatement _ (_, _, Assignment name value) = "$" ++ name ++ "=" ++ stringifyValue value ++ ";"
stringifyStatement _ (_, _, While condition block) = "while(" ++ show condition ++ ")" ++ stringifyStatement False block
stringifyStatement _ (_, _, IfChain chain else') = join "else " (map (\(cond, block) -> "if(" ++ stringifyValue cond ++ ")" ++ stringifyStatement False block) chain) ++ "else" ++ stringifyStatement False else'
stringifyStatement return (_, _, Block statements) = "{" ++ concat (map (stringifyStatement False) $ init statements) ++ (stringifyStatement return $ last statements) ++ "}"

stringifyValue :: Value -> String
stringifyValue (_, _, Literal num) = show num
stringifyValue (_, _, Variable name) = '$' : name
stringifyValue (_, _, Function args block) = ("(function(" ++ (join "," (map ('$':) argNames)) ++ ")use(" ++ captures ++ ")") ++ (stringifyStatement True block) ++ ")"
  where argNames = map fst args
        captures = join "," $ map ("&$" ++) $ Set.toList $ capturesOfBlock (Set.fromList argNames) [block]
stringifyValue (_, _, Operation (_, _, Primitive cmd) args) = stringifyPrimitive cmd args
stringifyValue (_, _, Operation fun args) = "(" ++ (stringifyValue fun) ++ "(" ++ (join "," $ map stringifyValue args) ++ "))"
stringifyValue (_, _, Primitive Not) = "(function($x){return !$x;})"

stringifyDeclaration :: String -> Value -> String
stringifyDeclaration name value = "$" ++ name ++ " = " ++ (stringifyValue value) ++ ";"

generate :: [(String, Value)] -> String
generate globals = concat $ map (uncurry stringifyDeclaration) globals