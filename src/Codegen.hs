module Codegen (generate) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Language (Type (..), Primitive (..), removePrimitives)
import Semantics (Expression (..), Statement (..))
import Errors (Fallible, failure)
import Functions (intercalate, split, number)
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

defaultValue :: Type -> String
defaultValue Void = "NULL"
defaultValue Int = "0"
defaultValue Bool = "FALSE"
defaultValue Float = "0.0"
defaultValue Text = "''"
defaultValue (Function args result) = "(function(" ++ intercalate "," (map (\n -> "$_" ++ show n) $ number args) ++ "){return " ++ defaultValue result ++ ";})"
defaultValue (IntArray _) = "z0array::n(array())"
defaultValue (MapArray _ _) = "z0array::n(array())"
defaultValue (Record fields) = "z0array::n(array(" ++ intercalate "," (map stringifyAssoc $ Map.assocs fields) ++ "))"
  where stringifyAssoc (name, value) = "'" ++ name ++ "'=>" ++ defaultValue value

stringifyPrimitive :: Primitive -> [String] -> [Type] -> String
stringifyPrimitive Add [a, b] [Int, Int] = "(int)(" ++ a ++ "+" ++ b ++ ")"
stringifyPrimitive Add [a, b] [Text, Text] = "(" ++ a ++ "." ++ b ++ ")"
stringifyPrimitive Add [a, b] [IntArray _, IntArray _] = a ++ "->add(" ++ b ++ ")"
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
stringifyPrimitive Or [a, b] [IntArray _, IntArray _] = a ++ "->union(" ++ b ++ ")"
stringifyPrimitive Or [a, b] [MapArray _ _, MapArray _ _] = a ++ "->union(" ++ b ++ ")"
stringifyPrimitive Or [a, b] [Record _, Record f2] = a ++ "->unionk(" ++ b ++ ",array(" ++ intercalate "," (Map.keys f2) ++ "))"
stringifyPrimitive Or [a, b] [Bool, Bool] = "(" ++ a ++ "||" ++ b ++ ")"
stringifyPrimitive Xor [a, b] [Bool, Bool] = "(" ++ a ++ "!==" ++ b ++ ")"
stringifyPrimitive Xor [a, b] [Int, Int] = "(" ++ a ++ "^" ++ b ++ ")"
stringifyPrimitive Eq [a, b] [IntArray _, IntArray _] = a ++ "->eq(" ++ b ++ ")"
stringifyPrimitive Eq [a, b] [MapArray _ _, MapArray _ _] = a ++ "->eq(" ++ b ++ ")"
stringifyPrimitive Eq [a, b] [_, _] = "(" ++ a ++ "===" ++ b ++ ")"
stringifyPrimitive Neq [a, b] [IntArray _, IntArray _] = "!" ++ a ++ "->eq(" ++ b ++ ")"
stringifyPrimitive Neq [a, b] [MapArray _ _, MapArray _ _] = "!" ++ a ++ "->eq(" ++ b ++ ")"
stringifyPrimitive Neq [a, b] [_, _] = "(" ++ a ++ "!==" ++ b ++ ")"
stringifyPrimitive Lt [a, b] [_, _] = "(" ++ a ++ "<" ++ b ++ ")"
stringifyPrimitive Gt [a, b] [_, _] = "(" ++ a ++ ">" ++ b ++ ")"
stringifyPrimitive Le [a, b] [_, _] = "(" ++ a ++ "<=" ++ b ++ ")"
stringifyPrimitive Ge [a, b] [_, _] = "(" ++ a ++ ">=" ++ b ++ ")"
stringifyPrimitive Pow [a, b] [Int, Int] = "(int)pow(" ++ a ++ "**" ++ b ++ ")"
stringifyPrimitive Pow [a, b] [_, _] = "pow(" ++ a ++ "," ++ b ++ ")"
stringifyPrimitive Not [a] [Int] = "(~" ++ a ++ ")"
stringifyPrimitive Not [a] [Bool] = "(!" ++ a ++ ")"
stringifyPrimitive AsInt [a] [_] = "(int)" ++ a
stringifyPrimitive AsBool [a] [Text] = "(" ++ a ++ "!=='')"
stringifyPrimitive AsBool [a] [_] = "(bool)" ++ a
stringifyPrimitive AsFloat [a] [_] = "(float)" ++ a
stringifyPrimitive AsText [a] [IntArray _] = "json_encode(" ++ a ++ ")"
stringifyPrimitive AsText [a] [MapArray _ _] = "json_encode(" ++ a ++ ")"
stringifyPrimitive AsText [a] [Record _] = "json_encode(" ++ a ++ ")"
stringifyPrimitive AsText [a] [_] = "(string)" ++ a
stringifyPrimitive Map (_ : _ : args) _ = "z0array::n(array(" ++ intercalate "," (map (\(k, v) -> k ++ "=>" ++ v) $ fromJust $ split args) ++ "))"
stringifyPrimitive Array (_ : args) _ = "z0array::n(array(" ++ intercalate "," args ++ "))"
stringifyPrimitive Get [array, key] [IntArray _, _] = array ++ "->get(" ++ key ++ ")"
stringifyPrimitive Get [array, key] [MapArray _ _, _] = array ++ "->getk(" ++ key ++ ")"
stringifyPrimitive Set [array, key, value] [IntArray _, _, _] = array ++ "->set(" ++ key ++ "," ++ value ++ ")"
stringifyPrimitive Set [array, key, value] [MapArray _ _, _, _] = array ++ "->setk(" ++ key ++ "," ++ value ++ ")"
stringifyPrimitive Has [array, key] [MapArray _ _, _] = array ++ "->hask(" ++ key ++ ")"
stringifyPrimitive Size [text] [Text] = "mb_strlen(" ++ text ++ ")"
stringifyPrimitive Size [array] [IntArray _] = "count(" ++ array ++ "->a)"
stringifyPrimitive Size [array] [MapArray _ _] = "count(" ++ array ++ "->a)"
stringifyPrimitive Concat (array : args) (IntArray v : types) = array ++ "->concat(" ++ intercalate "," (map mapping $ zip types args) ++ ")"
  where mapping (IntArray type', arg) | type' == v = "array(" ++ arg ++ ")"
        mapping (type', arg) = arg
stringifyPrimitive Sized [array, size] [IntArray v, Int] = array ++ "->sized(" ++ size ++ "," ++ defaultValue v ++ ")"
stringifyPrimitive Sized [array, size, value] [IntArray _, Int, _] = array ++ "->sized(" ++ size ++ "," ++ value ++ ")"

stringifyStatement :: Statement -> String
stringifyStatement (Expression value) = stringifyExpression value ++ ";"
stringifyStatement (Initialization name value) = "$z0" ++ name ++ "=" ++ stringifyExpression value ++ ";"
stringifyStatement (Assignment name value) = "$z0" ++ name ++ "=" ++ stringifyExpression value ++ ";"
stringifyStatement (While condition block) = "while(" ++ stringifyExpression condition ++ "){" ++ stringifyBlock False block ++ "}"
stringifyStatement (IfChain chain else') = intercalate "else " (map (\(cond, block) -> "if(" ++ stringifyExpression cond ++ "){" ++ stringifyBlock False block ++ "}") chain) ++ "else{" ++ stringifyBlock False else' ++ "}"

stringifyBlock :: Bool -> [Statement] -> String
stringifyBlock _ [] = ""
stringifyBlock True [statement@(Expression (type', _))] | type' /= Void = "return " ++ stringifyStatement statement
stringifyBlock return (statement : statements) = stringifyStatement statement ++ stringifyBlock return statements

stringifyExpression :: (Type, Expression) -> String
stringifyExpression (_, LiteralBool b) = if b then "TRUE" else "FALSE"
stringifyExpression (_, LiteralInt i) = show i
stringifyExpression (_, LiteralFloat f) = show f
stringifyExpression (_, LiteralText s) = show s
stringifyExpression (_, LiteralRecord fields) = "array(" ++ intercalate "," (map stringifyAssoc $ Map.assocs fields) ++ ")"
  where stringifyAssoc (name, value) = "'" ++ name ++ "'=>" ++ stringifyExpression value
stringifyExpression (_, Name name) = "$z0" ++ name
stringifyExpression (_, Call fun args) = stringifyExpression fun ++ "(" ++ (intercalate "," $ map stringifyExpression args) ++ ")"
stringifyExpression (_, Primitive primitive args) = stringifyPrimitive primitive (map stringifyExpression args) (map fst args)
stringifyExpression (Function _ returnType', Lambda args block) = header ++ "{" ++ stringifyBlock (returnType' /= Void) block ++ "})"
  where argNames = map fst args
        captures = intercalate "," $ Set.map ("&$z0" ++) $ removePrimitives $ capturesOfBlock (Set.fromList argNames) block
        header = "(function(" ++ intercalate "," (map ("$z0" ++) argNames) ++ ")" ++ (if null captures then "" else "use(" ++ captures ++ ")")
stringifyExpression (_, Access obj field) = stringifyExpression obj ++ "[\"" ++ field ++ "\"]"

stringifyDeclaration :: String -> (Type, Expression) -> String
stringifyDeclaration name value = "$z0" ++ name ++ "=" ++ (stringifyExpression value) ++ ";"

preamble :: String
preamble = "$z0int=0;$z0float=0.0;$z0bool=FALSE;$z0text='';$z0void=NULL;\
\class z1array{\
  \public $a;\
  \function static n($a) {\
    \$z=new z1array();\
    \$z->a=$a;\
    \return $z;\
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
  \public getk($i){\
    \if(!isset($this->a[$i])){\
      \die('Map does not contain the key '.$i);\
    \}\
    \return $this->a[$i];\
  \}\
  \public setk($i,$v){\
    \$this->a[$i]=$v;\
  \}\
  \public has(){\
    \return count($this->a)!==0;\
  \}\
  \public hask($i){\
    \return array_key_exists($i,$this->a);\
  \}\
  \public eq($x){\
    \$a=$this->a;\
    \$b=$x->a;\
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
  \public union($x){\
    \return z1array::n($x->a+$this->a);\
  \}\
  \public add($x){\
    \return z1array::n(array_merge($this->a,$x->a);\
  \}\
  \public concat(...$a){\
    \return z1array::n(array_merge($this->a, ...$a));\
  \}\
  \public unionk($x,$k){\
    \return z1array::n(array_intersect_key($x->a,$k)+$this->a);\
  \}\
  \public sized($n,$v){\
    \if($n<0){\
      \die('Array size must be non-negative, got '.$n);\
    \}\
    \if($n<count($this->a)){\
      \return z1array::n(array_slice($this->a,0,$n));\
    \}\
    \return z1array::n(array_pad($this->a,$n,$v));\
  \}\
\}"

generate :: [(String, (Type, Expression))] -> String
generate globals = preamble ++ concat (map (uncurry stringifyDeclaration) globals)