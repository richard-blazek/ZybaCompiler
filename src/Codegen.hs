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
defaultValue (Vector _) = "z1array::n([])"
defaultValue (Dictionary _ _) = "z1array::n([])"
defaultValue (Record fields) = "z1array::n([" ++ intercalate "," (map stringifyAssoc $ Map.assocs fields) ++ "])"
  where stringifyAssoc (name, value) = "'" ++ name ++ "'=>" ++ defaultValue value

asArrayArgument :: Type -> (Type, String) -> String
asArrayArgument itemType (Vector type', arg) | type' == itemType = arg
asArrayArgument itemType (type', arg) = "[" ++ arg ++ "]"

stringifyPrimitive :: Primitive -> [String] -> [Type] -> String
stringifyPrimitive Add [a, b] [Int, Int] = "(int)(" ++ a ++ "+" ++ b ++ ")"
stringifyPrimitive Add [a, b] [Text, Text] = "(" ++ a ++ "." ++ b ++ ")"
stringifyPrimitive Add [a, b] [Vector _, Vector _] = a ++ "->concat(" ++ b ++ ")"
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
stringifyPrimitive Or [a, b] [Dictionary _ _, Dictionary _ _] = a ++ "->unionD(" ++ b ++ ")"
stringifyPrimitive Or [a, b] [Record _, Record f2] = a ++ "->unionR(" ++ b ++ ",array(" ++ intercalate "," (Map.keys f2) ++ "))"
stringifyPrimitive Or [a, b] [Bool, Bool] = "(" ++ a ++ "||" ++ b ++ ")"
stringifyPrimitive Xor [a, b] [Bool, Bool] = "(" ++ a ++ "!==" ++ b ++ ")"
stringifyPrimitive Xor [a, b] [Int, Int] = "(" ++ a ++ "^" ++ b ++ ")"
stringifyPrimitive Eq _ [a, b] | a /= b = "FALSE"
stringifyPrimitive Eq [a, b] [_, _] = "(" ++ a ++ "==" ++ b ++ ")"
stringifyPrimitive Neq _ [a, b] | a /= b = "FALSE"
stringifyPrimitive Neq [a, b] [_, _] = "(" ++ a ++ "!=" ++ b ++ ")"
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
stringifyPrimitive AsText [a] [Text] = a
stringifyPrimitive AsText [a] [_] = "json_encode(" ++ a ++ ")"
stringifyPrimitive Dict (_ : _ : args) _ = "z1array::n([" ++ intercalate "," (map (\(k, v) -> k ++ "=>" ++ v) $ fromJust $ split args) ++ "])"
stringifyPrimitive List (_ : args) _ = "z1array::n([" ++ intercalate "," args ++ "])"
stringifyPrimitive Get [array, key] [Vector _, _] = array ++ "->getV(" ++ key ++ ")"
stringifyPrimitive Get [array, key] [Dictionary _ _, _] = array ++ "->getD(" ++ key ++ ")"
stringifyPrimitive Set [array, key, value] [Vector _, _, _] = array ++ "->setV(" ++ key ++ "," ++ value ++ ")"
stringifyPrimitive Set [array, key, value] [Dictionary _ _, _, _] = array ++ "->setD(" ++ key ++ "," ++ value ++ ")"
stringifyPrimitive Has [array, key] [Dictionary _ _, _] = array ++ "->hasD(" ++ key ++ ")"
stringifyPrimitive Size [text] [Text] = "mb_strlen(" ++ text ++ ")"
stringifyPrimitive Size [array] [Vector _] = "count(" ++ array ++ "->a)"
stringifyPrimitive Size [array] [Dictionary _ _] = "count(" ++ array ++ "->a)"
stringifyPrimitive Concat (array : args) (Vector v : types) = array ++ "->concat(" ++ intercalate "," (map (asArrayArgument v) $ zip types args) ++ ")"
stringifyPrimitive Append (array : args) (Vector v : types) = array ++ "->append(" ++ intercalate "," (map (asArrayArgument v) $ zip types args) ++ ")"
stringifyPrimitive Sized [array, size] [Vector v, Int] = array ++ "->sized(" ++ size ++ "," ++ defaultValue v ++ ")"
stringifyPrimitive Sized [array, size, value] [Vector _, Int, _] = array ++ "->sized(" ++ size ++ "," ++ value ++ ")"
stringifyPrimitive Sort [array] _ = array ++ "->sort(FALSE)"
stringifyPrimitive Sort [array, reversed] [Vector _, Bool] = array ++ "->sort(" ++ reversed ++ ")"
stringifyPrimitive Sort [array, compare] _ = array ++ "->usort(" ++ compare ++ ")"
stringifyPrimitive Join [array, separator] [Vector Text, Text] = "implode(" ++ separator ++ "," ++ array ++ "->a)"
stringifyPrimitive Join [array, separator] [Vector v, Text] = "implode(" ++ separator ++ ",array_map(" ++ array ++ "->a,'json_encode'))"

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
stringifyExpression (_, LiteralRecord fields) = "z1array::n([" ++ intercalate "," (map stringifyAssoc $ Map.assocs fields) ++ "])"
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
\class z1array implements JsonSerializable{\
  \public $a;\
  \function static n($a) {\
    \$z=new z1array();\
    \$z->a=$a;\
    \return $z;\
  \}\
  \public getV($i){\
    \$c=count($this->a);\
    \if($c===0){\
      \die('Array is empty, no element at '.$i);\
    \}\
    \return $this->a[($i%$c+$c)%$c];\
  \}\
  \public setV($i,$v){\
    \$c=count($this->a);\
    \if($c===0){\
      \die('Array is empty, no element at '.$i);\
    \}\
    \$this->a[($i%$c+$c)%$c]=$v;\
  \}\
  \public getD($i){\
    \if(!isset($this->a[$i])){\
      \die('Map does not contain the key '.$i);\
    \}\
    \return $this->a[$i];\
  \}\
  \public setD($i,$v){\
    \$this->a[$i]=$v;\
  \}\
  \public hasD($k){\
    \return array_key_exists($k,$this->a);\
  \}\
  \public unionD($x){\
    \return z1array::n($x->a+$this->a);\
  \}\
  \public concat(...$a){\
    \return z1array::n(array_merge($this->a, ...$a));\
  \}\
  \public append(...$a){\
    \$this->a=array_merge($this->a, ...$a);\
  \}\
  \public unionR($x,$k){\
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
  \public sort($r){\
    \if($r){\
      \rsort($this->a);\
    \}else{\
      \sort($this->a);\
    \}\
  \}\
  \public usort($c){\
    \usort($this->a,$c);\
  \}\
  \public jsonSerialize(){\
    \return $this->a;\
  \}\
\}"

generate :: [(String, (Type, Expression))] -> String
generate globals = preamble ++ concat (map (uncurry stringifyDeclaration) globals)