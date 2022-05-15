module Lang.Syntax (
  Expr(..),
  Ground(..),
  Type(..),
  Name
) where

type Name = String

data Ground
  = LInt Int
  | LBool Bool
  | LConst String
  deriving (Show, Eq, Ord)

data Expr
  = Var Name
  | Lit Ground
  | App Expr Expr
  | Lam Name Type Expr
  | Pred Name [Name]
  | Neg Expr
  | Conj Expr Expr
  | Disj Expr Expr
  | Impl Expr Expr
  | UnivQ Name Type Expr
  | ExisQ Name Type Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Eq  Expr Expr
  -- | Set
  -- | SetUnion
  -- | SetInter
  -- | SetDiff
  -- | SetCompl
  -- | SetCompr
  deriving (Eq, Show)

data Type
  = TInt
  | TEnt
  | TBool
  | TFunc Type Type
  deriving (Eq, Read, Show)