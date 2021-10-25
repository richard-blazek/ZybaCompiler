module Codegen (generate) where

import qualified Data.Map.Strict as Map
import Semantics
import Functions (apply, join)

generateValue :: Semantics.Value -> String
generateValue (Literal _ num) = show num
generateValue (Function _ args result) = "function(" ++ join "," (map ('$':) args) ++ "){return " ++ generateValue result ++ ";}"
generateValue (Global _ name) = '$' : name
generateValue (Argument _ name) = '$' : name
generateValue (Operation _ (Primitive _ cmd) args) = case (cmd, args) of
    (Add, [a, b]) -> "(" ++ generateValue a ++ "+" ++ generateValue b ++ ")"
    (Subtract, [a, b]) -> "(" ++ generateValue a ++ "-" ++ generateValue b ++ ")"
    (Multiply, [a, b]) -> "(" ++ generateValue a ++ "*" ++ generateValue b ++ ")"
    (Divide, [a, b]) -> "(" ++ generateValue a ++ "/" ++ generateValue b ++ ")"
    (Modulo, [a, b]) -> "(" ++ generateValue a ++ "%" ++ generateValue b ++ ")"
    (And, [a, b]) -> "(" ++ generateValue a ++ "&&" ++ generateValue b ++ ")"
    (Or, [a, b]) -> "(" ++ generateValue a ++ "||" ++ generateValue b ++ ")"
    (Xor, [a, b]) -> "(" ++ generateValue a ++ "^" ++ generateValue b ++ ")"
    (Equal, [a, b]) -> "(" ++ generateValue a ++ "==" ++ generateValue b ++ ")"
    (NotEqual, [a, b]) -> "(" ++ generateValue a ++ "!=" ++ generateValue b ++ ")"
    (LowerThan, [a, b]) -> "(" ++ generateValue a ++ "<" ++ generateValue b ++ ")"
    (GreaterThan, [a, b]) -> "(" ++ generateValue a ++ ">" ++ generateValue b ++ ")"
    (Power, [a, b]) -> "(pow(" ++ generateValue a ++ "," ++ generateValue b ++ "))"
    (Not, [a]) ->  "(!" ++ generateValue a ++ ")"
    (If, [a, b, c]) -> "(" ++ generateValue a ++ "?" ++ generateValue b ++ ":" ++ generateValue c ++ ")"
    x -> "(##Compile error - unknown operation " ++ show x ++ "##)"

generateValue (Operation _ fn args) = "(" ++ generateValue fn ++ "(" ++ join "," (map generateValue args) ++ "))"
generateValue (InvalidValue msg) = "(##Compile error - invalid value " ++ msg ++ "##)"

generateGlobalValue :: String -> Semantics.Value -> String
generateGlobalValue name value = "$" ++ name ++ " = " ++ generateValue value ++ ";"

generate :: Semantics.Scope -> String
generate scope = concat $ map (apply generateGlobalValue) $ Map.assocs scope