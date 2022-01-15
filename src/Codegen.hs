module Codegen (gen) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Language as Lang
import Semantics (Value (..), Statement (..))
import Parser (Literal (..))
import Functions (intercalate, split, pad)

capturesOfBlock :: (String -> String) -> String -> Set.Set String -> [Statement] -> Set.Set String
capturesOfBlock _ _ _ [] = Set.empty
capturesOfBlock qual this skip (Value value : rest) = capturesOfValue qual this skip value
capturesOfBlock qual this skip (Initialization name value : rest) = Set.union (capturesOfValue qual this skip value) $ capturesOfBlock qual this (Set.insert name skip) rest
capturesOfBlock qual this skip (Assignment _ value : rest) = Set.union (capturesOfValue qual this skip value) $ capturesOfBlock qual this skip rest
capturesOfBlock qual this skip (While condition block : rest) = Set.unions [capturesOfValue qual this skip condition, capturesOfBlock qual this skip block, capturesOfBlock qual this skip rest]
capturesOfBlock qual this skip (IfChain chains else' : rest) = Set.unions [Set.unions $ map (\(cond, block) -> Set.union (capturesOfValue qual this skip cond) $ capturesOfBlock qual this skip block) chains, capturesOfBlock qual this skip else', capturesOfBlock qual this skip rest]

capturesOfValue :: (String -> String) -> String -> Set.Set String -> (Lang.Type, Value) -> Set.Set String
capturesOfValue qual _ skip (_, Name pkg name) = Set.difference (Set.singleton $ qual pkg ++ name) skip
capturesOfValue qual this skip (_, Lambda args body) = capturesOfBlock qual this (Set.union skip $ Set.fromList $ map ((qual this ++) . fst) args) body
capturesOfValue qual this skip (_, Call fun args) = Set.unions $ map (capturesOfValue qual this skip) $ fun : args
capturesOfValue _ _ skip _ = Set.empty

defaultValue :: Lang.Type -> String
defaultValue Lang.Void = "NULL"
defaultValue Lang.Int = "0"
defaultValue Lang.Bool = "FALSE"
defaultValue Lang.Real = "0.0"
defaultValue Lang.Text = "''"
defaultValue (Lang.Function args result) = "(function(" ++ intercalate "," (zipWith (const . ("$_" ++) . show) [0..] args) ++ "){return " ++ defaultValue result ++ ";})"
defaultValue (Lang.Vector _) = "z1array::n([])"
defaultValue (Lang.Dictionary _ _) = "z1array::n([])"
defaultValue (Lang.Record fields) = "z1array::n([" ++ intercalate "," (map genAssoc $ Map.assocs fields) ++ "])"
  where genAssoc (name, value) = "'" ++ name ++ "'=>" ++ defaultValue value

asArrayArgument :: Lang.Type -> (Lang.Type, String) -> String
asArrayArgument itemType (Lang.Vector type', arg) | type' == itemType = arg
asArrayArgument itemType (type', arg) = "[" ++ arg ++ "]"

genBuiltin :: Lang.Builtin -> [String] -> [Lang.Type] -> String
genBuiltin Lang.Add [a, b] [Lang.Int, Lang.Int] = "(int)(" ++ a ++ "+" ++ b ++ ")"
genBuiltin Lang.Add [a, b] [Lang.Text, Lang.Text] = "(" ++ a ++ "." ++ b ++ ")"
genBuiltin Lang.Add [a, b] [Lang.Vector _, Lang.Vector _] = a ++ "->concat(" ++ b ++ ")"
genBuiltin Lang.Add [a, b] [_, _] = "(" ++ a ++ "+" ++ b ++ ")"
genBuiltin Lang.Sub [a, b] [Lang.Int, Lang.Int] = "(int)(" ++ a ++ "-" ++ b ++ ")"
genBuiltin Lang.Sub [a, b] [_, _] = "(" ++ a ++ "-" ++ b ++ ")"
genBuiltin Lang.Mul [a, b] [Lang.Int, Lang.Int] = "(int)(" ++ a ++ "*" ++ b ++ ")"
genBuiltin Lang.Mul [a, b] [_, _] = "(" ++ a ++ "*" ++ b ++ ")"
genBuiltin Lang.Div [a, b] [_, _] = "(" ++ a ++ "/(float)" ++ b ++ ")"
genBuiltin Lang.IntDiv [a, b] [_, _] = "(int)(" ++ a ++ "/" ++ b ++ ")"
genBuiltin Lang.Rem [a, b] [Lang.Int, Lang.Int] = "(" ++ a ++ "%" ++ b ++ ")"
genBuiltin Lang.Rem [a, b] [_, _] = "fmod(" ++ a ++ "," ++ b ++ ")"
genBuiltin Lang.And [a, b] [Lang.Int, Lang.Int] = "(" ++ a ++ "&" ++ b ++ ")"
genBuiltin Lang.And [a, b] [_, _] = "(" ++ a ++ "&&" ++ b ++ ")"
genBuiltin Lang.Or [a, b] [Lang.Int, Lang.Int] = "(" ++ a ++ "||" ++ b ++ ")"
genBuiltin Lang.Or [a, b] [Lang.Dictionary _ _, Lang.Dictionary _ _] = a ++ "->unionD(" ++ b ++ ")"
genBuiltin Lang.Or [a, b] [Lang.Record _, Lang.Record f2] = a ++ "->unionR(" ++ b ++ ",array(" ++ intercalate "," (Map.keys f2) ++ "))"
genBuiltin Lang.Or [a, b] [Lang.Bool, Lang.Bool] = "(" ++ a ++ "||" ++ b ++ ")"
genBuiltin Lang.Xor [a, b] [Lang.Bool, Lang.Bool] = "(" ++ a ++ "!==" ++ b ++ ")"
genBuiltin Lang.Xor [a, b] [Lang.Int, Lang.Int] = "(" ++ a ++ "^" ++ b ++ ")"
genBuiltin Lang.Eq _ [a, b] | a /= b = "FALSE"
genBuiltin Lang.Eq [a, b] [_, _] = "(" ++ a ++ "==" ++ b ++ ")"
genBuiltin Lang.Neq _ [a, b] | a /= b = "FALSE"
genBuiltin Lang.Neq [a, b] [_, _] = "(" ++ a ++ "!=" ++ b ++ ")"
genBuiltin Lang.Lt [a, b] [_, _] = "(" ++ a ++ "<" ++ b ++ ")"
genBuiltin Lang.Gt [a, b] [_, _] = "(" ++ a ++ ">" ++ b ++ ")"
genBuiltin Lang.Le [a, b] [_, _] = "(" ++ a ++ "<=" ++ b ++ ")"
genBuiltin Lang.Ge [a, b] [_, _] = "(" ++ a ++ ">=" ++ b ++ ")"
genBuiltin Lang.Pow [a, b] [Lang.Int, Lang.Int] = "(int)pow(" ++ a ++ "**" ++ b ++ ")"
genBuiltin Lang.Pow [a, b] [_, _] = "pow(" ++ a ++ "," ++ b ++ ")"
genBuiltin Lang.Not [a] [Lang.Int] = "(~" ++ a ++ ")"
genBuiltin Lang.Not [a] [Lang.Bool] = "(!" ++ a ++ ")"
genBuiltin Lang.AsInt [a] [_] = "(int)" ++ a
genBuiltin Lang.AsBool [a] [Lang.Text] = "(" ++ a ++ "!=='')"
genBuiltin Lang.AsBool [a] [_] = "(bool)" ++ a
genBuiltin Lang.AsReal [a] [_] = "(float)" ++ a
genBuiltin Lang.AsText [a] [Lang.Text] = a
genBuiltin Lang.AsText [a] [_] = "json_encode(" ++ a ++ ")"
genBuiltin Lang.Dict (_ : _ : args) _ = "z1array::n([" ++ intercalate "," (map (\(k, v) -> k ++ "=>" ++ v) $ fst $ split args) ++ "])"
genBuiltin Lang.List (_ : args) _ = "z1array::n([" ++ intercalate "," args ++ "])"
genBuiltin Lang.Get [array, key] [Lang.Vector _, _] = array ++ "->getV(" ++ key ++ ")"
genBuiltin Lang.Get [array, key] [Lang.Dictionary _ _, _] = array ++ "->getD(" ++ key ++ ")"
genBuiltin Lang.Set [array, key, value] [Lang.Vector _, _, _] = array ++ "->setV(" ++ key ++ "," ++ value ++ ")"
genBuiltin Lang.Set [array, key, value] [Lang.Dictionary _ _, _, _] = array ++ "->setD(" ++ key ++ "," ++ value ++ ")"
genBuiltin Lang.Has [array, key] [Lang.Dictionary _ _, _] = array ++ "->hasD(" ++ key ++ ")"
genBuiltin Lang.Size [text] [Lang.Text] = "mb_strlen(" ++ text ++ ")"
genBuiltin Lang.Size [array] [Lang.Vector _] = "count(" ++ array ++ "->a)"
genBuiltin Lang.Size [array] [Lang.Dictionary _ _] = "count(" ++ array ++ "->a)"
genBuiltin Lang.Concat (array : args) (Lang.Vector v : types) = array ++ "->concat(" ++ intercalate "," (map (asArrayArgument v) $ zip types args) ++ ")"
genBuiltin Lang.Append (array : args) (Lang.Vector v : types) = array ++ "->append(" ++ intercalate "," (map (asArrayArgument v) $ zip types args) ++ ")"
genBuiltin Lang.Sized [array, size] [Lang.Vector v, Lang.Int] = array ++ "->sized(" ++ size ++ "," ++ defaultValue v ++ ")"
genBuiltin Lang.Sized [array, size, value] [Lang.Vector _, Lang.Int, _] = array ++ "->sized(" ++ size ++ "," ++ value ++ ")"
genBuiltin Lang.Sort [array] _ = array ++ "->sort(FALSE)"
genBuiltin Lang.Sort [array, reversed] [Lang.Vector _, Lang.Bool] = array ++ "->sortV(" ++ reversed ++ ")"
genBuiltin Lang.Sort [array, compare] _ = array ++ "->usortV(" ++ compare ++ ")"
genBuiltin Lang.Join [array, separator] [Lang.Vector Lang.Text, Lang.Text] = "implode(" ++ separator ++ "," ++ array ++ "->a)"
genBuiltin Lang.Join [array, separator] [Lang.Vector v, Lang.Text] = "implode(" ++ separator ++ ",array_map(" ++ array ++ "->a,'json_encode'))"

genStatement :: (String -> String) -> String -> Statement -> String
genStatement qual this (Value value) = genValue qual this value ++ ";"
genStatement qual this (Initialization name value) = qual this ++ name ++ "=" ++ genValue qual this value ++ ";"
genStatement qual this (Assignment name value) = qual this ++ name ++ "=" ++ genValue qual this value ++ ";"
genStatement qual this (While condition block) = "while(" ++ genValue qual this condition ++ "){" ++ genBlock qual this False block ++ "}"
genStatement qual this (IfChain chain else') = intercalate "else " (map genIf chain) ++ "else{" ++ genBlock qual this False else' ++ "}"
  where genIf (cond, block) = "if(" ++ genValue qual this cond ++ "){" ++ genBlock qual this False block ++ "}"

genBlock :: (String -> String) -> String -> Bool -> [Statement] -> String
genBlock _ _ _ [] = ""
genBlock qual this True [statement@(Value (type', _))] | type' /= Lang.Void = "return " ++ genStatement qual this statement
genBlock qual this return (statement : statements) = genStatement qual this statement ++ genBlock qual this return statements

genValue :: (String -> String) -> String -> (Lang.Type, Value) -> String
genValue _ _ (_, Literal (Bool b)) = if b then "TRUE" else "FALSE"
genValue _ _ (_, Literal (Int i)) = show i
genValue _ _ (_, Literal (Real r)) = show r
genValue _ _ (_, Literal (Text s)) = show s
genValue qual this (_, Name pkg name) = qual pkg ++ name
genValue qual this (_, Call fun args) = genValue qual this fun ++ "(" ++ (intercalate "," $ map (genValue qual this) args) ++ ")"
genValue qual this (_, Builtin builtin args) = genBuiltin builtin (map (genValue qual this) args) (map fst args)
genValue qual this (_, Record fields) = "z1array::n([" ++ intercalate "," (map genAssoc $ Map.assocs fields) ++ "])"
  where genAssoc (name, value) = "'" ++ name ++ "'=>" ++ genValue qual this value
genValue qual this (Lang.Function _ returnType', Lambda args block) = header ++ "{" ++ genBlock qual this (returnType' /= Lang.Void) block ++ "})"
  where argNames = map ((qual this ++) . fst) args
        captures = intercalate "," $ Set.map ('&' :) $ Lang.removeBuiltins $ capturesOfBlock qual this (Set.fromList argNames) block
        header = "(function(" ++ intercalate "," argNames ++ ")" ++ (if null captures then "" else "use(" ++ captures ++ ")")
genValue qual this (_, Access obj field) = genValue qual this obj ++ "[\"" ++ field ++ "\"]"

genDeclaration :: (String -> String) -> String -> String -> (Lang.Type, Semantics.Value) -> String
genDeclaration qual this name value = qual this ++ name ++ "=" ++ genValue qual this value ++ ";"

preamble :: [String] -> String
preamble quals = "<?php " ++ concat (map (\q -> concat $ map (q ++) ["int=0;", "real=0.0;", "bool=FALSE;", "text='';", "void=NULL;"]) quals) ++ "\
\class z1array implements JsonSerializable{\
  \public $a;\
  \public static function n($a) {\
    \$z=new z1array();\
    \$z->a=$a;\
    \return $z;\
  \}\
  \public function getV($i){\
    \$c=count($this->a);\
    \if($c==0){\
      \die('An empty array cannot have an index '.$i);\
    \}\
    \return $this->a[(($i%$c)+$c)%$c];\
  \}\
  \public function setV($i,$v){\
    \$c=count($this->a);\
    \if($c==0){\
      \die('An empty array cannot have an index '.$i);\
    \}\
    \$this->a[(($i%$c)+$c)%$c]=$v;\
  \}\
  \public function getD($i){\
    \if(!array_key_exists($i,$this->a)){\
      \die('Map does not contain the key '.$i);\
    \}\
    \return $this->a[$i];\
  \}\
  \public function setD($i,$v){\
    \$this->a[$i]=$v;\
  \}\
  \public function hasD($k){\
    \return array_key_exists($k,$this->a);\
  \}\
  \public function unionD($x){\
    \return z1array::n($x->a+$this->a);\
  \}\
  \public function concat(...$a){\
    \return z1array::n(array_merge($this->a, ...$a));\
  \}\
  \public function append(...$a){\
    \$this->a=array_merge($this->a, ...$a);\
  \}\
  \public function unionR($x,$k){\
    \return z1array::n(array_intersect_key($x->a,$k)+$this->a);\
  \}\
  \public function sized($n,$v){\
    \if($n<0){\
      \die('Cannot resize an array to a negative size '.$n);\
    \}\
    \if($n<count($this->a)){\
      \return z1array::n(array_slice($this->a,0,$n));\
    \}\
    \return z1array::n(array_pad($this->a,$n,$v));\
  \}\
  \public function sortV($r){\
    \if($r){\
      \rsort($this->a);\
    \}else{\
      \sort($this->a);\
    \}\
  \}\
  \public function usortV($c){\
    \usort($this->a,$c);\
  \}\
  \public function jsonSerialize(){\
    \return $this->a;\
  \}\
\}?>"

genPkg :: (String -> String) -> String -> [(String, (Lang.Type, Semantics.Value))] -> String
genPkg qual pkg decls = "<?php " ++ concat (map (uncurry $ genDeclaration qual pkg) decls) ++ "?>"

gen :: [String] -> [(String, [(String, (Lang.Type, Semantics.Value))])] -> String
gen phps pkgs = concat phps ++ preamble qualified ++ concat (map (uncurry $ genPkg (quals Map.!)) pkgs)
  where qualified = let len = length pkgs in map (("$z0" ++) . pad (length $ show len) '0' . show) [0..len-1]
        quals = Map.fromList $ zip (reverse $ map fst pkgs) qualified
