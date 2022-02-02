module Codegen (gen) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Language as Lang
import Semantics (Value (..), TypedValue, Statement (..))
import Parser (Literal (..))
import Functions (intercalate, split, pad)

capturesOfBlock :: (String -> String) -> String -> Set.Set String -> [Statement] -> Set.Set String
capturesOfBlock _ _ _ [] = Set.empty
capturesOfBlock qual this skip (Value value : rest) = capturesOfValue qual this skip value
capturesOfBlock qual this skip (Initialization name value : rest) = Set.union (capturesOfValue qual this skip value) $ capturesOfBlock qual this (Set.insert name skip) rest
capturesOfBlock qual this skip (Assignment _ value : rest) = Set.union (capturesOfValue qual this skip value) $ capturesOfBlock qual this skip rest
capturesOfBlock qual this skip (While condition block : rest) = Set.unions [capturesOfValue qual this skip condition, capturesOfBlock qual this skip block, capturesOfBlock qual this skip rest]
capturesOfBlock qual this skip (IfChain chains else' : rest) = Set.unions [Set.unions $ map (\(cond, block) -> Set.union (capturesOfValue qual this skip cond) $ capturesOfBlock qual this skip block) chains, capturesOfBlock qual this skip else', capturesOfBlock qual this skip rest]

capturesOfValue :: (String -> String) -> String -> Set.Set String -> TypedValue -> Set.Set String
capturesOfValue qual _ skip (Name pkg name, _) = Set.difference (Set.singleton $ qual pkg ++ name) skip
capturesOfValue qual this skip (Lambda args body, _) = capturesOfBlock qual this (Set.union skip $ Set.fromList $ map ((qual this ++) . fst) args) body
capturesOfValue qual this skip (Call fun args, _) = Set.unions $ map (capturesOfValue qual this skip) $ fun : args
capturesOfValue _ _ skip _ = Set.empty

genDefault :: Lang.Type -> String
genDefault Lang.Void = "NULL"
genDefault Lang.Int = "0"
genDefault Lang.Bool = "FALSE"
genDefault Lang.Real = "0.0"
genDefault Lang.Text = "''"
genDefault (Lang.Function args result) = "(function(" ++ intercalate "," (zipWith (const . ("$_" ++) . show) [0..] args) ++ "){return " ++ genDefault result ++ ";})"
genDefault (Lang.Vector _) = "z1array::n([])"
genDefault (Lang.Dictionary _ _) = "z1array::n([])"
genDefault (Lang.Record fields) = "z1array::n([" ++ intercalate "," (map genAssoc $ Map.assocs fields) ++ "])"
  where genAssoc (name, value) = "'" ++ name ++ "'=>" ++ genDefault value

asArrayArgument :: Lang.Type -> (Lang.Type, String) -> String
asArrayArgument itemType (Lang.Vector type', arg) | type' == itemType = arg
asArrayArgument itemType (type', arg) = "[" ++ arg ++ "]"

genBuiltin :: Lang.Builtin -> [String] -> [Lang.Type] -> String
genBuiltin Lang.Add [a, b] [Lang.Int, Lang.Int] = "(int)(" ++ a ++ "+" ++ b ++ ")"
genBuiltin Lang.Add [a, b] [Lang.Text, Lang.Text] = "(" ++ a ++ "." ++ b ++ ")"
genBuiltin Lang.Add [a, b] [Lang.Vector _, Lang.Vector _] = a ++ "->concatV(" ++ b ++ ")"
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
genBuiltin Lang.Or [a, b] [Lang.Dictionary _ _, Lang.Dictionary _ _] = a ++ "->interD(" ++ b ++ ")"
genBuiltin Lang.Or [a, b] [Lang.Record _, Lang.Record _] = a ++ "->interD(" ++ b ++ ")"
genBuiltin Lang.Or [a, b] [Lang.Int, Lang.Int] = "(" ++ a ++ "||" ++ b ++ ")"
genBuiltin Lang.Or [a, b] [Lang.Dictionary _ _, Lang.Dictionary _ _] = a ++ "->unionD(" ++ b ++ ")"
genBuiltin Lang.Or [a, b] [Lang.Record _, Lang.Record _] = a ++ "->unionD(" ++ b ++ ")"
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
genBuiltin Lang.AsText [a] [_] = "json_encode(" ++ a ++ ")"
genBuiltin Lang.Dict (_ : _ : args) _ = "z1array::n([" ++ intercalate "," (map (\(k, v) -> k ++ "=>" ++ v) $ fst $ split args) ++ "])"
genBuiltin Lang.List (_ : args) _ = "z1array::n([" ++ intercalate "," args ++ "])"
genBuiltin Lang.Fun _ (returned : args) = genDefault $ Lang.Function args returned
genBuiltin Lang.Get [array, index] [Lang.Vector _, _] = array ++ "->getV(" ++ index ++ ")"
genBuiltin Lang.Get [array, index, length] [_, _, _] = array ++ "->sliceV(" ++ index ++ "," ++ length ++ ")"
genBuiltin Lang.Get [array, key] [Lang.Dictionary _ _, _] = array ++ "->getD(" ++ key ++ ")"
genBuiltin Lang.Set [array, index, value] [Lang.Vector _, _, _] = array ++ "->setV(" ++ index ++ "," ++ value ++ ")"
genBuiltin Lang.Set [array, key, value] [Lang.Dictionary _ _, _, _] = array ++ "->setD(" ++ key ++ "," ++ value ++ ")"
genBuiltin Lang.Has [array, key] [Lang.Dictionary _ _, _] = array ++ "->hasD(" ++ key ++ ")"
genBuiltin Lang.Count [text] [Lang.Text] = "mb_strlen(" ++ text ++ ",'UTF-8')"
genBuiltin Lang.Count [array] _ = "count(" ++ array ++ "->a)"
genBuiltin Lang.Count [text, predicate] [Lang.Text, _] = "z1count(mb_str_split(" ++ text ++ ",1,'UTF-8')," ++ predicate ++ ")"
genBuiltin Lang.Count [array, predicate] _ = "z1count(" ++ array ++ "->a," ++ predicate ++ ")"
genBuiltin Lang.Concat (array : args) (Lang.Vector v : types) = array ++ "->concatV(" ++ intercalate "," (map (asArrayArgument v) $ zip types args) ++ ")"
genBuiltin Lang.Pad [array, size] [Lang.Vector v, _] = array ++ "->padV(" ++ size ++ "," ++ genDefault v ++ ")"
genBuiltin Lang.Pad [array, size, value] [Lang.Vector _, _, _] = array ++ "->padV(" ++ size ++ "," ++ value ++ ")"
genBuiltin Lang.Pad [array, size, value, padLeft] [Lang.Vector _, _, _] = array ++ "->padV(" ++ size ++ "," ++ value ++ "," ++ padLeft ++ ")"
genBuiltin Lang.Pad [text, size] [Lang.Text, _] = "str_pad(" ++ text ++ "," ++ size ++ ",' ')"
genBuiltin Lang.Pad [text, size, value] [Lang.Text, _, _] = "str_pad(" ++ text ++ "," ++ size ++ "," ++ value ++ ")"
genBuiltin Lang.Pad [text, size, value, padLeft] [Lang.Text, _, _, _] = "str_pad(" ++ text ++ "," ++ size ++ "," ++ value ++ "," ++ padLeft ++ "?STR_PAD_LEFT:STR_PAD_RIGHT)"
genBuiltin Lang.Sort [array] _ = array ++ "->sort(FALSE)"
genBuiltin Lang.Sort [array, reversed] [Lang.Vector _, Lang.Bool] = array ++ "->sortV(" ++ reversed ++ ")"
genBuiltin Lang.Sort [array, compare] _ = array ++ "->usortV(" ++ compare ++ ")"
genBuiltin Lang.Join [array, separator] [Lang.Vector Lang.Text, Lang.Text] = "implode(" ++ separator ++ "," ++ array ++ "->a)"
genBuiltin Lang.Join [array, separator] [Lang.Vector v, Lang.Text] = "implode(" ++ separator ++ ",array_map(" ++ array ++ "->a,'json_encode'))"
genBuiltin Lang.Insert [array, index, values] [Lang.Vector v, _, Lang.Vector v2] | v == v2 = array ++ "->insertV(" ++ index ++ "," ++ values ++ "->a)"
genBuiltin Lang.Insert [array, index, value] [Lang.Vector _, _, _] = array ++ "->insertV(" ++ index ++ ",[" ++ value ++ "])"
genBuiltin Lang.Erase [array, index] [Lang.Vector _, _] = array ++ "->eraseV(" ++ index ++ ",1)"
genBuiltin Lang.Erase [array, index, length] [Lang.Vector _, _, _] = array ++ "->eraseV(" ++ index ++ "," ++ length ++ ")"
genBuiltin Lang.Erase [array, index] [Lang.Dictionary _ _, _] = "unset(" ++ array ++ "->a[" ++ index ++ "])"
genBuiltin Lang.Append (array : args) (Lang.Vector v : types) = array ++ "->appendV(" ++ intercalate "," (map (asArrayArgument v) $ zip types args) ++ ")"
genBuiltin Lang.Remove [array, value] [Lang.Vector _, _] = array ++ "->remove(" ++ value ++ ")"
genBuiltin Lang.Remove [array, value] [Lang.Dictionary _ _, _] = array ++ "->remove(" ++ value ++ ")"
genBuiltin Lang.Find [array, value] [Lang.Vector _, _] = array ++ "->find(" ++ value ++ ",-1)"
genBuiltin Lang.Find [array, value] [Lang.Dictionary k _, _] = array ++ "->find(" ++ value ++ "," ++ genDefault k ++ ")"
genBuiltin Lang.AsList [text] [Lang.Text] = "z1array::n(mb_str_split(" ++ text ++ ",1,'UTF-8'))"
genBuiltin Lang.AsList [array] [Lang.Dictionary _ _] = array ++ "->asList()"
genBuiltin Lang.AsDict [array] [Lang.Vector _] = array ++ "->asDict()"
genBuiltin Lang.Map (fn : arrays) _ = "z1array::n(array_map(" ++ fn ++ "," ++ intercalate "," (map (++ "->a") arrays) ++ "))"
genBuiltin Lang.Filter [text, pred] [Lang.Text, _] = "implode('',array_filter(mb_str_split(" ++ text ++ ",1,'UTF-8')," ++ pred ++ "))"
genBuiltin Lang.Filter [array, pred] _ = "z1array::n(array_filter(" ++ array ++ "," ++ pred ++ "))"
genBuiltin Lang.Fold [text, seed, op] [Lang.Text, _] = "array_reduce(mb_str_split(" ++ text ++ ",1,'UTF-8')," ++ op ++ "," ++ seed ++ ")"
genBuiltin Lang.Fold [array, seed, op] _ = "array_reduce(" ++ array ++ "->a," ++ op ++ "," ++ seed ++ ")"
genBuiltin Lang.Keys [array] _ = "z1array::n(array_keys(" ++ array ++ "->a))"
genBuiltin Lang.Values [array] _ = "z1array::n(array_values(" ++ array ++ "->a))"
genBuiltin Lang.Flat [array] _ = "z1array::n(array_merge(...(" ++ array ++ "->a)))"
genBuiltin Lang.Shuffle [array] _ = "shuffle(" ++ array ++ "->a)"
genBuiltin Lang.Contains [text, element] [Lang.Text, _] = "(strpos(" ++ text ++ "," ++ element ++ ")!==FALSE)"
genBuiltin Lang.Contains [array, element] _ = "(array_search(" ++ element ++ "," ++ array ++ "->a)!==FALSE)"
genBuiltin Lang.Split [text, separator] _ = "z1array::n(mb_split(preg_quote(" ++ separator ++ ")," ++ text ++ "))"

genStatement :: (String -> String) -> String -> Bool -> Statement -> String
genStatement qual this return (Value value) = (if return then "return " else "") ++ genValue qual this value ++ ";"
genStatement qual this _ (Initialization name value) = qual this ++ name ++ "=" ++ genValue qual this value ++ ";"
genStatement qual this _ (Assignment name value) = qual this ++ name ++ "=" ++ genValue qual this value ++ ";"
genStatement qual this _ (While condition block) = "while(" ++ genValue qual this condition ++ "){" ++ genBlock qual this False block ++ "}"
genStatement qual this _ (Return value) = "return " ++ genValue qual this value ++ ";"
genStatement qual this _ (IfChain chain else') = intercalate "else " (map genIf chain) ++ "else{" ++ genBlock qual this False else' ++ "}"
  where genIf (cond, block) = "if(" ++ genValue qual this cond ++ "){" ++ genBlock qual this False block ++ "}"
genStatement qual this _ (For name count block) = "for(" ++ i ++ "=0;" ++ i ++ "<=" ++ genValue qual this count ++ ";++" ++ i ++ "){" ++ genBlock qual this False block ++ "}"
  where i = qual this ++ name
genStatement qual this _ (Foreach valueName key iterable block) = case key of
  Nothing -> "foreach(" ++ genValue qual this iterable ++ " as " ++ qual this ++ valueName ++ "){" ++ genBlock qual this False block ++ "}"
  Just keyName -> "foreach(" ++ genValue qual this iterable ++ " as " ++ qual this ++ valueName ++ "=>" ++ qual this ++ keyName ++ "){" ++ genBlock qual this False block ++ "}"

genBlock :: (String -> String) -> String -> Bool -> [Statement] -> String
genBlock _ _ _ [] = ""
genBlock qual this True [statement@(Value (_, type'))] = genStatement qual this (type' /= Lang.Void) statement
genBlock qual this return (statement : statements) = genStatement qual this False statement ++ genBlock qual this return statements

genValue :: (String -> String) -> String -> TypedValue -> String
genValue _ _ (Literal (Bool b), _) = if b then "TRUE" else "FALSE"
genValue _ _ (Literal (Int i), _) = show i
genValue _ _ (Literal (Real r), _) = show r
genValue _ _ (Literal (Text s), _) = show s
genValue qual this (Name pkg name, _) = qual pkg ++ name
genValue qual this (Call fun args, _) = genValue qual this fun ++ "(" ++ (intercalate "," $ map (genValue qual this) args) ++ ")"
genValue qual this (Builtin builtin args, _) = genBuiltin builtin (map (genValue qual this) args) $ map snd args
genValue qual this (Record fields, _) = "z1array::n([" ++ intercalate "," (map genAssoc $ Map.assocs fields) ++ "])"
  where genAssoc (name, value) = "'" ++ name ++ "'=>" ++ genValue qual this value
genValue qual this (Lambda args block, Lang.Function _ returnType') = header ++ "{" ++ genBlock qual this (returnType' /= Lang.Void) block ++ "})"
  where argNames = map ((qual this ++) . fst) args
        captures = intercalate "," $ Set.map ('&' :) $ Lang.removeBuiltins $ capturesOfBlock qual this (Set.fromList argNames) block
        header = "(function(" ++ intercalate "," argNames ++ ")" ++ (if null captures then "" else "use(" ++ captures ++ ")")
genValue qual this (Access obj field, _) = genValue qual this obj ++ "[\"" ++ field ++ "\"]"
genValue qual this (PhpValue name, _) = '$' : name

genDeclaration :: (String -> String) -> String -> String -> TypedValue -> String
genDeclaration qual this name value = qual this ++ name ++ "=" ++ genValue qual this value ++ ";"

preamble :: [String] -> String
preamble quals = "<?php " ++ concat (map (\q -> concat $ map (q ++) ["int=0;", "real=0.0;", "bool=FALSE;", "text='';", "void=NULL;"]) quals) ++ "\
\session_start();\
\mb_internal_encoding('UTF-8');\
\mb_regex_encoding('UTF-8');\
\function z1count($a,$p){\
  \$r=0;\
  \foreach($a as $v){\
    \$r=(int)($r+$p($v));\
  \}\
  \return $r;\
\}\
\class z1array implements JsonSerializable{\
  \public $a;\
  \public static function n($a) {\
    \$z=new z1array();\
    \$z->a=$a;\
    \return $z;\
  \}\
  \public function jsonSerialize(){\
    \return $this->a;\
  \}\
  \public function getV($i){\
    \$c=count($this->a);\
    \if(i>=$c||i<0){\
      \die('List of size '.$c.' does not have the index '.$i);\
    \}\
    \return $this->a[$i];\
  \}\
  \public function setV($i,$v){\
    \$c=count($this->a);\
    \if($i>$c){\
      \array_splice($this->a,count($this->a),0,array_fill(0,$i-$c+1,$v));\
    \}else if($i<0){\
      \array_splice($this->a,0,0,array_fill(0,-$i,$v));\
    \}else{\
      \$this->a[$i]=$v;\
    \}\
  \}\
  \public function getD($i){\
    \if(!array_key_exists($i,$this->a)){\
      \die('Dict does not contain the key '.$i);\
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
  \public function interD($x){\
    \return z1array::n(array_intersect_key($x->a,$this->a));\
  \}\
  \public function concatV(...$a){\
    \return z1array::n(array_merge($this->a, ...$a));\
  \}\
  \public function padV($n,$v,$l=FALSE){\
    \$c=count($this->a);\
    \if($n<=$c){\
      \$b=$this->a;\
      \return z1array::n($b);\
    \}\
    \return z1array::n(array_pad($this->a,$l?-$n:$n,$v));\
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
  \public function insertV($i,$v){\
    \$n=count($v);\
    \if($n>0){\
      \$c=count($this->a);\
      \if($i>$c){\
        \array_splice($this->a,$c,0,array_pad($v,-($i-$c+$n),$v[0]));\
      \}else if($i<0){\
        \array_splice($this->a,0,0,array_pad($v,(-$i+$n),$v[$n-1]));\
      \}else{\
        \array_splice($this->a,$i,0,$v);\
      \}\
    \}\
  \}\
  \public function eraseV($i,$l){\
    \if($i<0){\
      \$l+=$i;\
      \$i=0;\
    \}\
    \array_splice($this->a,$i,$l<0?0:$l);\
  \}\
  \public function appendV(...$a){\
    \array_push($this->a,array_merge(...$a));\
  \}\
  \public function sliceV($i,$l){\
    \if($i<0){\
      \$l+=$i;\
      \$i=0;\
    \}\
    \return array_slice($this->a,$i,$l<0?0:$l);\
  \}\
  \public function remove($v){\
    \if(($k=array_search($v,$this->a))!==false){\
     \unset($this->a[$k]);\
    \}\
  \}\
  \public function find($v,$d){\
    \$s=array_search($v,$this->a);\
    \return $s===false?$d:$s;\
  \}\
  \public function asList(){\
    \$r=[];\
    \foreach($this->a as $k=>$v){\
      \$r[]=z1array::n(['key'=>$k,'value'=>$v]);\
    \}\
    \return z1array::n($r);\
  \}\
  \public function asDict($a){\
    \$r=[];\
    \foreach($this->a as $v){\
      \$r[$v->a['key']]=$v->a['value'];\
    \}\
    \return z1array::n($r);\
  \}\
\}?>"

genPkg :: (String -> String) -> String -> [(String, TypedValue)] -> String
genPkg qual pkg decls = "<?php " ++ concat (map (uncurry $ genDeclaration qual pkg) decls) ++ "?>"

gen :: [String] -> [(String, [(String, TypedValue)])] -> String
gen phps pkgs = concat phps ++ preamble qualified ++ concat (map (uncurry $ genPkg (quals Map.!)) pkgs)
  where qualified = let len = length pkgs in map (("$z0" ++) . pad (length $ show len) '0' . show) [0..len-1]
        quals = Map.fromList $ zip (reverse $ map fst pkgs) qualified
