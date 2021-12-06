module Codegen (generate) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Semantics
import Errors (Fallible, failure)
import Functions (join)

requiredCaptures :: [String] -> Set.Set String -> [Value] -> [String]
requiredCaptures skip captures [] = Set.toList $ Set.difference captures $ Set.fromList skip
requiredCaptures skip captures ((_, _, Variable name) : rest) = requiredCaptures skip (Set.insert name captures) rest
requiredCaptures skip captures ((_, _, Function args result) : rest) = requiredCaptures skip captures $ result : rest
requiredCaptures skip captures ((_, _, Operation fun args) : rest) = requiredCaptures skip captures $ fun : (args ++ rest)
requiredCaptures skip captures (_ : rest) = requiredCaptures skip captures rest

stringifyPrimitive :: Primitive -> [Value] -> Fallible String
stringifyPrimitive cmd args = do
    argStrings <- mapM stringifyValue args
    return $ case (cmd, argStrings) of
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
        (If, [a, b, c]) -> "(" ++ a ++ "?" ++ b ++ ":" ++ c ++ ")"

stringifyValue :: Value -> Fallible String
stringifyValue (_, _, Literal num) = Right $ show num
stringifyValue (_, _, Variable name) = Right $ '$' : name
stringifyValue (_, _, Function args result) = do
    captures <- return $ join "," $ map ("&$" ++) $ requiredCaptures args Set.empty [result]
    header <- return $ "function(" ++ (join "," (map ('$':) args)) ++ ")use(" ++ captures ++ ")"
    resultString <- stringifyValue result
    Right $ header ++ "{return " ++ resultString ++ ";}"
stringifyValue (_, _, Operation (_, _, Primitive cmd) args) = stringifyPrimitive cmd args
stringifyValue (_, _, Operation fun args) = do
    funString <- stringifyValue fun
    argStrings <- mapM stringifyValue args
    return $ "(" ++ funString ++ "(" ++ (join "," argStrings) ++ "))"
stringifyValue (line, _, Primitive cmd) = failure line $ "Unexpected primitive: " ++ show cmd

stringifyAssignment :: String -> Value -> Fallible String
stringifyAssignment name value = do
    valueString <- stringifyValue value
    Right $ "$" ++ name ++ " = " ++ valueString ++ ";"

generate :: Semantics.Scope -> Fallible String
generate scope = fmap concat $ mapM (uncurry stringifyAssignment) $ Map.assocs scope