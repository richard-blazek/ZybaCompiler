module Codegen (generate) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Language (Type (..), Primitive (..), removePrimitives)
import Semantics (Expression (..), Statement (..))
import Errors (Fallible, failure)
import Functions (intercalate, split)
import Data.Maybe (fromJust)

capturesOfBlock :: Set.Set String -> [Statement] -> Set.Set String
capturesOfBlock skip [] = Set.empty
capturesOfBlock skip (Expression value : rest) = capturesOfExpression skip value
capturesOfBlock skip (Initialization name value : rest) = Set.union (capturesOfExpression skip value) $ capturesOfBlock (Set.insert name skip) rest
capturesOfBlock skip (Assignment _ value : rest) = Set.union (capturesOfExpression skip value) $ capturesOfBlock skip rest
capturesOfBlock skip (While condition block : rest) = Set.unions [capturesOfExpression skip condition, capturesOfBlock skip block, capturesOfBlock skip rest]
capturesOfBlock skip (IfChain chains else' : rest) = Set.unions [Set.unions $ map (\(cond, block) -> Set.union (capturesOfExpression skip cond) $ capturesOfBlock skip block) chains, capturesOfBlock skip else', capturesOfBlock skip rest]

capturesOfExpression :: Set.Set String -> (Type, Expression) -> Set.Set String
capturesOfExpression skip (_, Name name) = if Set.member name skip then Set.empty else Set.singleton name
capturesOfExpression skip (_, Lambda args body) = capturesOfBlock (Set.union skip $ Set.fromList $ map fst args) body
capturesOfExpression skip (_, Call fun args) = Set.unions $ map (capturesOfExpression skip) $ fun : args
capturesOfExpression skip _ = Set.empty

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
stringifyPrimitive Neq [a, b] [MapArray _ _, MapArray _ _] = "!zyba0arrayEQ(" ++ a ++ "," ++ b ++ ")"
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
stringifyPrimitive Map (_ : _ : args) _ = "array(" ++ intercalate "," (map (\(k, v) -> k ++ "=>" ++ v) $ fromJust $ split args) ++ ")"
stringifyPrimitive Array (_ : args) _ = "(new zybaarray(" ++ intercalate "," args ++ "))"
stringifyPrimitive Get [array, key] (MapArray _ _ : _) = "(" ++ array ++ "[" ++ key ++ "] or die('Map does not contain the key: ' . (" ++ key ++ ")))"
stringifyPrimitive Set [array, key, value] (MapArray _ _ : _) = "(unset)(" ++ array ++ "[" ++ key ++ "]=" ++ value ++ ")"
stringifyPrimitive Get [array, key] (IntArray _ : _) = array ++ "->get(" ++ key ++ ")"
stringifyPrimitive Set [array, key, value] (IntArray _ : _) = array ++ "->set(" ++ key ++ "," ++ value ++ ")"

stringifyStatement :: Statement -> String
stringifyStatement (Expression value) = stringifyExpression value ++ ";"
stringifyStatement (Initialization name value) = "$zyba" ++ name ++ "=" ++ stringifyExpression value ++ ";"
stringifyStatement (Assignment name value) = "$zyba" ++ name ++ "=" ++ stringifyExpression value ++ ";"
stringifyStatement (While condition block) = "while(" ++ stringifyExpression condition ++ ")" ++ stringifyBlock False block
stringifyStatement (IfChain chain else') = intercalate "else " (map (\(cond, block) -> "if(" ++ stringifyExpression cond ++ ")" ++ stringifyBlock False block) chain) ++ "else" ++ stringifyBlock False else'

stringifyBlock :: Bool -> [Statement] -> String
stringifyBlock _ [] = ""
stringifyBlock True [statement@(Expression (type', _))] | type' /= Void = "return " ++ stringifyStatement statement
stringifyBlock return (statement : statements) = stringifyStatement statement ++ stringifyBlock return statements

stringifyExpression :: (Type, Expression) -> String
stringifyExpression (_, LiteralBool b) = if b then "TRUE" else "FALSE"
stringifyExpression (_, LiteralInt i) = show i
stringifyExpression (_, LiteralFloat f) = show f
stringifyExpression (_, LiteralText s) = show s
stringifyExpression (_, LiteralRecord fields) = "array(" ++ intercalate "," (map stringifyItem $ Map.assocs fields) ++ ")"
  where stringifyItem (name, value) = "\"" ++ name ++ "\"=>" ++ stringifyExpression value
stringifyExpression (_, Name name) = "$zyba" ++ name
stringifyExpression (_, Call fun args) = stringifyExpression fun ++ "(" ++ (intercalate "," $ map stringifyExpression args) ++ ")"
stringifyExpression (_, Primitive primitive args) = stringifyPrimitive primitive (map stringifyExpression args) (map fst args)
stringifyExpression (Function _ returnType', Lambda args block) = header ++ "{" ++ stringifyBlock (returnType' /= Void) block ++ "})"
  where argNames = map fst args
        captures = intercalate "," $ Set.map ("&$zyba" ++) $ removePrimitives $ capturesOfBlock (Set.fromList argNames) block
        header = "(function(" ++ intercalate "," (map ("$zyba" ++) argNames) ++ ")" ++ (if null captures then "" else "use(" ++ captures ++ ")")
stringifyExpression (_, Access obj field) = stringifyExpression obj ++ "[\"" ++ field ++ "\"]"

stringifyDeclaration :: String -> (Type, Expression) -> String
stringifyDeclaration name value = "$zyba" ++ name ++ "=" ++ (stringifyExpression value) ++ ";"

preamble :: String
preamble = "function zyba0arrayEQ($a,$b){\
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
\$zybaint=0;$zybafloat=0.0;$zybabool=FALSE;$zybatext='';$zybavoid=NULL;\
\class zybaarray{\
  \public $a;\
  \function __construct(...$e) {\
    \$this->a = $e;\
  \}\
  \public get($i){\
    \$c=count($this->a);\
    \if($c===0){\
      \die('Array is empty, no element at '.$i);\
    \}\
    \return $this->a[($i%$c+$c)%$c];\
  \}\
  \public set($i,$v){\
    \$c=count($this->a);\
    \if($c===0){\
      \die('Array is empty, no element at '.$i);\
    \}\
    \$this->a[($i%$c+$c)%$c]=$v;\
  \}\
\}"

generate :: [(String, (Type, Expression))] -> String
generate globals = preamble ++ concat (map (uncurry stringifyDeclaration) globals)