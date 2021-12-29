module Codegen (generate) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Language (Type (..), Primitive (..), removePrimitives)
import Semantics (Value (..), Statement (..))
import Errors (Fallible, failure)
import Functions (join)

capturesOfBlock :: Set.Set String -> [Statement] -> Set.Set String
capturesOfBlock skip [] = Set.empty
capturesOfBlock skip (Expression value : rest) = capturesOfValue skip value
capturesOfBlock skip (Initialization name value : rest) = Set.union (capturesOfValue skip value) $ capturesOfBlock (Set.insert name skip) rest
capturesOfBlock skip (Assignment _ value : rest) = Set.union (capturesOfValue skip value) $ capturesOfBlock skip rest
capturesOfBlock skip (While condition block : rest) = Set.unions [capturesOfValue skip condition, capturesOfBlock skip block, capturesOfBlock skip rest]
capturesOfBlock skip (IfChain chains else' : rest) = Set.unions [Set.unions $ map (\(cond, block) -> Set.union (capturesOfValue skip cond) $ capturesOfBlock skip block) chains, capturesOfBlock skip else', capturesOfBlock skip rest]

capturesOfValue :: Set.Set String -> (Type, Value) -> Set.Set String
capturesOfValue skip (_, Name name) = if Set.member name skip then Set.empty else Set.singleton name
capturesOfValue skip (_, Lambda args body) = capturesOfBlock (Set.union skip $ Set.fromList $ map fst args) body
capturesOfValue skip (_, Call fun args) = Set.unions $ map (capturesOfValue skip) $ fun : args
capturesOfValue skip _ = Set.empty

stringifyPrimitive :: Primitive -> [String] -> [Type] -> String
stringifyPrimitive Add [a, b] [Int, Int] = "(int)(" ++ a ++ "+" ++ b ++ ")"
stringifyPrimitive Add [a, b] [Text, Text] = "(" ++ a ++ "." ++ b ++ ")"
stringifyPrimitive Add [a, b] [_, _] = "(" ++ a ++ "+" ++ b ++ ")"
stringifyPrimitive Sub [a, b] [Int, Int] = "(int)(" ++ a ++ "-" ++ b ++ ")"
stringifyPrimitive Sub [a, b] [_, _] = "(" ++ a ++ "-" ++ b ++ ")"
stringifyPrimitive Mul [a, b] [Int, Int] = "(int)(" ++ a ++ "*" ++ b ++ ")"
stringifyPrimitive Mul [a, b] [_, _] = "(" ++ a ++ "*" ++ b ++ ")"
stringifyPrimitive Div [a, b] [_, _] = "(" ++ a ++ "/(float)" ++ b ++ ")"
stringifyPrimitive IntDiv [a, b] [_, _] = "(int)(" ++ a ++ "/" ++ b ++ ")"
stringifyPrimitive Rem [a, b] [Int, Int] = "(" ++ a ++ "%" ++ b ++ ")"
stringifyPrimitive Rem [a, b] [_, _] = "fmod(" ++ a ++ "," ++ b ++ ")"
stringifyPrimitive And [a, b] [Int, Int] = "(" ++ a ++ "&" ++ b ++ ")"
stringifyPrimitive And [a, b] [_, _] = "(" ++ a ++ "&&" ++ b ++ ")"
stringifyPrimitive Or [a, b] [Int, Int] = "(" ++ a ++ "||" ++ b ++ ")"
stringifyPrimitive Or [a, b] [_, _] = "(" ++ a ++ "||" ++ b ++ ")"
stringifyPrimitive Xor [a, b] [Bool, Bool] = "(" ++ a ++ "!==" ++ b ++ ")"
stringifyPrimitive Xor [a, b] [_, _] = "(" ++ a ++ "^" ++ b ++ ")"
stringifyPrimitive Eq [a, b] [MapArray _ _, MapArray _ _] = "zyba0arrayEQ(" ++ a ++ "," ++ b ++ ")"
stringifyPrimitive Eq [a, b] [_, _] = "(" ++ a ++ "===" ++ b ++ ")"
stringifyPrimitive Neq [a, b] [_, _] = "(" ++ a ++ "!==" ++ b ++ ")"
stringifyPrimitive Lt [a, b] [_, _] = "(" ++ a ++ "<" ++ b ++ ")"
stringifyPrimitive Gt [a, b] [_, _] = "(" ++ a ++ ">" ++ b ++ ")"
stringifyPrimitive Le [a, b] [_, _] = "(" ++ a ++ "<=" ++ b ++ ")"
stringifyPrimitive Ge [a, b] [_, _] = "(" ++ a ++ ">=" ++ b ++ ")"
stringifyPrimitive Pow [a, b] [Int, Int] = "(int)pow(" ++ a ++ "**" ++ b ++ ")"
stringifyPrimitive Pow [a, b] [_, _] = "pow(" ++ a ++ "," ++ b ++ ")"
stringifyPrimitive Not [a] [Int] = "(~" ++ a ++ ")"
stringifyPrimitive Not [a] [_] = "(!" ++ a ++ ")"
stringifyPrimitive AsInt [a] [_] = "(int)" ++ a
stringifyPrimitive AsBool [a] [Text] = "(" ++ a ++ "!==\"\")"
stringifyPrimitive AsBool [a] [_] = "(bool)" ++ a
stringifyPrimitive AsFloat [a] [_] = "(float)" ++ a
stringifyPrimitive AsText [a] [_] = "(string)" ++ a

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

stringifyValue :: (Type, Value) -> String
stringifyValue (_, LiteralBool b) = if b then "TRUE" else "FALSE"
stringifyValue (_, LiteralInt i) = show i
stringifyValue (_, LiteralFloat f) = show f
stringifyValue (_, LiteralText s) = show s
stringifyValue (_, Name name) = "$zyba" ++ name
stringifyValue (_, Call fun args) = stringifyValue fun ++ "(" ++ (join "," $ map stringifyValue args) ++ ")"
stringifyValue (_, Primitive primitive args) = stringifyPrimitive primitive (map stringifyValue args) (map fst args)
stringifyValue (Function _ returnType', Lambda args block) = header ++ "{" ++ stringifyBlock (returnType' /= Void) block ++ "})"
  where argNames = map fst args
        captures = join "," $ Set.map ("&$zyba" ++) $ removePrimitives $ capturesOfBlock (Set.fromList argNames) block
        header = "(function(" ++ join "," (map ("$zyba" ++) argNames) ++ ")" ++ (if null captures then "" else "use(" ++ captures ++ ")")
stringifyValue (_, Semantics.Record fields) = "array(" ++ join "," (map stringify $ Map.toList fields) ++ ")"
  where stringify (name, value) = "\"" ++ name ++ "\"=>" ++ stringifyValue value

stringifyDeclaration :: String -> (Type, Value) -> String
stringifyDeclaration name value = "$zyba" ++ name ++ "=" ++ (stringifyValue value) ++ ";"

preamble :: String
preamble = "function zyba0arrayEQ($a, $b) {\
    \if(!is_array($a)||!is_array($b)||count($a)!==count($b)){\
        \return FALSE;\
    \}\
    \$a_keys=array_keys($a);\
    \$b_keys=array_keys($b);\
    \array_multisort($a_keys);\
    \array_multisort($b_keys);\
    \if($a_keys!==$b_keys) {\
        \return FALSE;\
    \}\
    \foreach($a_keys as $key){\
        \$a_value=$a[$key];\
        \$b_value=$b[$key];\
        \if($a_value!==$b_value&&!array_eq($a_value,$b_value)){\
            \return FALSE;\
        \}\
    \}\
    \return TRUE;\
\}\
\$zybaint=0;$zybafloat=0.0;$zybabool=FALSE;$zybatext='';$zybavoid=NULL;"

generate :: [(String, (Type, Value))] -> String
generate globals = preamble ++ concat (map (uncurry stringifyDeclaration) globals)