module Interpret.Comp where

import qualified Compile.Syn as Syn
import qualified Compile.Sem as Sem
import qualified Compile.Type as Ty
import qualified Compile.Frag as F

data BTree a b = Branch a (BTree, BTree) | Leaf b | Ep

type SemNode = CN Syn.Type Syn.Expr Sem.Value
type SemTree = BTree SemNode String
type SynTree = BTree String String

compose :: F.Fragment -> SynTree -> SemTree
compose = _