module Lib.Std (
  idFn,
  idTy
) where

import qualified Compiler.Syntax as Syn

idTy = Syn.TyVar $ Syn.TV "<A,A>"
idFn = Syn.Lam "x" idTy (Syn.Var "x")