module Lang.Syn (
  Name,
  Lit(..),
  Term(..),
  Expr(..),
  Type(..),
) where

type Name = String

-- | Ground terms; those without free variables.
data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Term
  = TVar Name
  | TConst Name
  deriving (Eq, Show)

data Expr
  = Eq Expr Expr
  | ELit Lit
  | ETerm Term
  | Lam Name Type Expr
  | App Expr Expr
  | Pred Name [Term]
  | Neg Expr
  | Conj Expr Expr
  | Disj Expr Expr
  | Impl Expr Expr
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
  | TyBool
  | TyFunc Type Type
  deriving (Eq, Read, Show)