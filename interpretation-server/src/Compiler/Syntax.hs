{-# LANGUAGE PatternSynonyms #-}

module Compiler.Syntax (
  Name,
  Lit(..),
  Sym(..),
  Expr(..),
  UnOp(..),
  BinOp(..),
  Type(..),
  Decl,
  rename,
  substitute,
  tyInt,
  tyBool,
  pattern TyIntP,
  pattern TyBoolP
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
  = ELit Lit
  | ESym Sym
  | Lam Name Type Expr
  | App Expr Expr
  | Let Sym Expr
  | EBinOp BinOp
  | EUnOp UnOp
  | Pred Name [Expr]
  -- TODO: Name/Type should be Expr to allow arbitrary restriction
  | UnivQ Name Type Expr
  | ExisQ Name Type Expr
  | IotaQ Name Type Expr
  | Set [Expr]
  | SetUnion [Expr] [Expr]
  | SetInter [Expr] [Expr]
  | SetDiff [Expr] [Expr]
  | SetCompl [Expr] [Expr]
  | SetMem Expr [Expr]

data UnOp = Neg Expr

data BinOp
  = Eq Expr Expr
  | Conj Expr Expr
  | Disj Expr Expr
  | Impl Expr Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr

newtype TyVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TyVar TyVar
  | TyCon String
  | TyFunc Type Type
  deriving (Eq, Ord)

tyInt, tyBool :: Type
tyInt  = TyCon "n"
tyBool = TyCon "t"

pattern TyIntP = TyCon "n"
pattern TyBoolP = TyCon "t"

type Decl = (String, Expr)

-- rename n to n' in e
rename :: Name -> Name -> Expr -> Expr
rename n n' e = case e of
  ESym (SVar v)   | n == v -> ESym (SVar n')
  ESym (SConst c) | n == c -> ESym (SConst n')
  Pred name args  -> Pred (if n == name then n' else name) (map rn args)
  EUnOp op        -> EUnOp $ renameUnOp op
  EBinOp op       -> EBinOp $ renameBinOp op
  Lam arg ty e'   -> Lam arg ty (rn e')
  App e0 e1       -> App (rn e0) (rn e1)
  _               -> e
  where
    rn = rename n n'
    renameUnOp op = case op of
      Neg e -> Neg (rn e)
    -- yikes! there must be a better way to do this
    renameBinOp op = case op of
      Eq e0 e1 -> Eq (rn e0) (rn e1)
      Conj e0 e1 -> Conj (rn e0) (rn e1)
      Disj e0 e1 -> Disj (rn e0) (rn e1)
      Impl e0 e1 -> Impl (rn e0) (rn e1)
      Add e0 e1 -> Add (rn e0) (rn e1)
      Mul e0 e1 -> Mul (rn e0) (rn e1)
      Sub e0 e1 -> Sub (rn e0) (rn e1)
      Div e0 e1 -> Div (rn e0) (rn e1)

-- sub a for every match(n) in e
substitute' :: Expr -> (Name -> Bool) -> Expr -> Expr
substitute' a match e = case e of
  ESym (SVar n) | match n -> a
  Pred name args          -> Pred name (map sub args)
  EUnOp op                -> EUnOp $ subUnOp op
  EBinOp op               -> EBinOp $ subBinOp op
  Lam arg ty body         -> Lam arg ty (substitute' a (\n -> match n && n /= arg) body)
  _ -> e
  where
    sub = substitute' a match
    subUnOp op = case op of
      Neg e -> Neg (sub e)
    subBinOp op = case op of
      Eq e0 e1 -> Eq (sub e0) (sub e1)
      Conj e0 e1 -> Conj (sub e0) (sub e1)
      Disj e0 e1 -> Disj (sub e0) (sub e1)
      Impl e0 e1 -> Impl (sub e0) (sub e1)
      Add e0 e1 -> Add (sub e0) (sub e1)
      Mul e0 e1 -> Mul (sub e0) (sub e1)
      Sub e0 e1 -> Sub (sub e0) (sub e1)
      Div e0 e1 -> Div (sub e0) (sub e1)

substitute :: Expr -> Name -> Expr -> Expr
substitute a n e = substitute' a (\n' -> n == n') e
