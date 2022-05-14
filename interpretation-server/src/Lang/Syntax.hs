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
  deriving (Show, Eq, Ord)

data Expr
  = Var Name
  | Lit Ground
  | App Expr Expr
  | Lam Name Type Expr
  | Pred Name [Name]
  | Not Expr
  | And [Expr]
  | Or  [Expr]
  | Impl Expr Expr
  | ForAll Name Expr
  | Exists Name Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Eq  Expr Expr
  deriving (Eq, Show)

data Type
  = TInt
  | TBool
  | TArr Type Type
  deriving (Eq, Read, Show)