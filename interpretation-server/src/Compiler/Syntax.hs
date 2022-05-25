module Compiler.Syntax (
  Name,
  Lit(..),
  Sym(..),
  Expr(..),
  Type(..),
  Decl
) where

type Name = String

-- | Ground terms; those without free variables.
data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

-- | Symbols. Variables are bound in an evaluation context,
--   constants in a semantic model.
data Sym
  = SVar Name
  | SConst Name
  deriving (Eq, Show)

data Expr
  = Eq Expr Expr
  | ELit Lit
  | ESym Sym
  | Lam Name Type Expr
  | App Expr Expr
  | Let Sym Expr
  -- TODO: predicate argument type should be
  -- [Expr] to accommodate e.g. functions that return symbols
  | Pred Name [Sym]
  | Neg Expr
  | Conj Expr Expr
  | Disj Expr Expr
  | Impl Expr Expr
  -- TODO: Name/Type should be Expr to allow arbitrary restriction
  | UnivQ Name Type Expr
  | ExisQ Name Type Expr
  | IotaQ Name Type Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Set [Expr]
  | SetUnion [Expr] [Expr]
  | SetInter [Expr] [Expr]
  | SetDiff [Expr] [Expr]
  | SetCompl [Expr] [Expr]
  | SetMem Expr [Expr]

data Type
  = TyInt
  | TyEnt
  | TyEvent
  | TyBool
  | TyTyVar
  | TyFunc Type Type
  deriving (Eq, Read)

type Decl = (String, Expr)
