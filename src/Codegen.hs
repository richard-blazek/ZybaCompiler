module Codegen (generate) where

import qualified Semantics
import qualified Data.Map.Strict as Map
import Functions (apply)

generateGlobalValue :: String -> Semantics.Value -> String
generateGlobalValue name value = "$" ++ name ++ " = " ++ show value ++ ";"

generate :: Semantics.Scope -> String
generate scope = concat $ map (apply generateGlobalValue) $ Map.assocs scope