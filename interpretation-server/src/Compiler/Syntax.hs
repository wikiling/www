{-# LANGUAGE PatternSynonyms #-}

module Compiler.Syntax (
  Name,
  Lit(..),
  Expr(..),
  UnOp(..),
  BinOp(..),
  Type(..),
  TyVar(..),
  TyScheme(..),
  Decl(..),
  Quant(..),
  SetExpr,
  rename,
  substitute,
  tyInt,
  tyBool,
  pattern TyIntP,
  pattern TyBoolP,
  mkSet
) where

import qualified Data.Set as Set

type Name = String

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Expr
  = ELit Lit
  | Var Name
  | Const Name Type
  | Lam Name Type Expr
  | App Expr Expr
  | EBinOp BinOp Expr Expr
  | EUnOp UnOp Expr
  | Pred Name [Expr]
  | EQuant Quant Name Type Expr
  | ESet SetExpr
  deriving (Eq, Ord)

type SetExpr = Set.Set Expr

mkSet :: [Expr] -> Expr
mkSet exprs = ESet $ Set.fromList exprs

data Quant = Univ | Exis | Iota deriving (Eq,Ord)

data UnOp
  = Neg
  | SetCompl
  deriving (Eq, Ord)

data BinOp
  = Eq
  | Conj
  | Disj
  | Impl
  | Add
  | Mul
  | Sub
  | Div
  | SetUnion
  | SetInter
  | SetDiff
  | SetSubS
  | SetPropSubS
  | SetMem
  deriving (Eq, Ord)

newtype TyVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TyVar TyVar
  | TyCon String
  | TyFun Type Type
  deriving (Eq, Ord)

tyInt, tyBool :: Type
tyInt  = TyCon "n"
tyBool = TyCon "t"

pattern TyIntP = TyCon "n"
pattern TyBoolP = TyCon "t"

data TyScheme = Forall [TyVar] Type
  deriving (Eq, Ord)

data Decl = Let Name Expr | Typedef Name Type

-- rename n to n' in e
rename :: Name -> Name -> Expr -> Expr
rename n n' e = case e of
  Var v     | n == v -> Var n'
  Const c t | n == c -> Const n' t
  Pred name args  -> Pred (if n == name then n' else name) (map rn args)
  EUnOp op e'     -> EUnOp op (rn e')
  EBinOp op e0 e1 -> EBinOp op (rn e0) (rn e1)
  Lam arg ty e'   -> Lam arg ty (rn e')
  EQuant q n t e' -> EQuant q n t (rn e')
  App e0 e1       -> App (rn e0) (rn e1)
  _               -> e
  where
    rn = rename n n'

-- sub a for every match(n) in e
substitute' :: Expr -> (Name -> Bool) -> Expr -> Expr
substitute' a match e = case e of
  Var n | match n     -> a
  Pred name args      -> Pred name (map sub args)
  EUnOp op e'         -> EUnOp op (sub e')
  EBinOp op e0 e1     -> EBinOp op (sub e0) (sub e1)
  Lam arg ty body     -> Lam arg ty (substitute' a (\n -> match n && n /= arg) body)
  EQuant q n t e'     -> EQuant q n t (substitute' a (\n' -> match n' && n' /= n) e')
  App e0 e1           -> App (sub e0) (sub e1)
  _ -> e
  where
    sub = substitute' a match

substitute :: Expr -> Name -> Expr -> Expr
substitute a n e = substitute' a (\n' -> n == n') e
