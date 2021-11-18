module Codegen (generate) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Semantics
import Functions (join)

requiredCaptures :: [String] -> Set.Set String -> [Value] -> [String]
requiredCaptures skip captures [] = Set.toList $ Set.difference captures $ Set.fromList skip
requiredCaptures skip captures (Variable _ name : rest) = requiredCaptures skip (Set.insert name captures) rest
requiredCaptures skip captures (Function _ args result : rest) = requiredCaptures skip captures $ result : rest
requiredCaptures skip captures (Operation _ fn args : rest) = requiredCaptures skip captures $ fn : (args ++ rest)
requiredCaptures skip captures (_ : rest) = requiredCaptures skip captures rest

generatePrimitive :: Primitive -> [Value] -> String
generatePrimitive cmd args = case (cmd, args) of
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

generateValue :: Value -> String
generateValue (Literal _ num) = show num
generateValue (Function _ args result) = header ++ "{return " ++ generateValue result ++ ";}"
    where
        header = "function(" ++ (join "," (map ('$':) args)) ++ ")use(" ++ captureString ++ ")"
        captureString = join "," $ map ('&':'$':) $ requiredCaptures args Set.empty [result]
generateValue (Variable _ name) = '$' : name
generateValue (Operation _ fn args) = case fn of
    Primitive _ cmd -> generatePrimitive cmd args
    _ -> "(" ++ generateValue fn ++ "(" ++ join "," (map generateValue args) ++ "))"
generateValue (InvalidValue msg) = "(##Compile error - invalid value " ++ msg ++ "##)"
generateValue (Primitive _ cmd) = "(##Compile error - unexpected primitive " ++ show cmd ++ "##)"

generateGlobalValue :: String -> Semantics.Value -> String
generateGlobalValue name value = "$" ++ name ++ " = " ++ generateValue value ++ ";"

generate :: Semantics.Scope -> String
generate scope = concat $ map (uncurry generateGlobalValue) $ Map.assocs scope