module Interpret.Frag where

import qualified Data.Map as Map
import qualified Compile.Syn as Syn

type Fragment = Map.Map String (Syn.Expr, Syn.Type)
