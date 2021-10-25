module Codegen (generate) where

import qualified Semantics

generate :: Semantics.Scope -> String
generate scope = show scope