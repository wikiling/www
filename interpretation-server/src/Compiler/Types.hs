module Compiler.Types (
  check,
  checkExpr,
  TypeError(..),
  Ctx
) where

import qualified Data.Set as Set
import qualified Compiler.Syntax as Syn

import Control.Monad.Except
import Control.Monad.Reader

import Debug.Trace (traceM)

type Ctx = [(Syn.Name, Syn.Type)]

extend :: (Syn.Name, Syn.Type) -> Ctx -> Ctx
extend xt ctx = xt : ctx

data TypeError
  = Mismatch Syn.Type Syn.Type
  | NotFunction Syn.Expr Syn.Expr
  | NotInScope Syn.Name
  | HeterogenousSet Syn.SetExpr

type Check = ExceptT TypeError (Reader Ctx)

inCtx :: (Syn.Name, Syn.Type) -> Check a -> Check a
inCtx (x,t) = local (extend (x,t))

lookupVar :: Syn.Name -> Check Syn.Type
lookupVar n = do
  ctx <- ask
  case lookup n ctx of
    Just e  -> pure e
    Nothing -> throwError $ NotInScope n

checkBinaryOp :: Syn.Type -> Syn.Expr -> Syn.Expr -> Check Syn.Type
checkBinaryOp t e1 e2 = do
  t1 <- check e1
  t2 <- check e2
  if t1 == t
    then if t2 == t
      then pure t
      else throwError $  Mismatch t2 t
    else throwError $  Mismatch t1 t

checkQuant :: Syn.Name -> Syn.Type -> Syn.Expr -> Check Syn.Type 
checkQuant n t e = do
    bodyT <- inCtx (n,t) (check e)
    if bodyT == Syn.tyBool
      then pure Syn.tyBool
      else throwError $ Mismatch bodyT Syn.tyBool

check :: Syn.Expr -> Check Syn.Type
check expr = case expr of
  Syn.ELit Syn.LInt{} -> pure Syn.tyInt
  Syn.ELit Syn.LBool{} -> pure Syn.tyBool

  Syn.EConst n t -> pure t
  Syn.EVar n -> lookupVar n

  Syn.EUnOp op -> case op of
    Syn.Neg e -> do
      t <- check e
      case t of
        Syn.TyBoolP -> pure Syn.tyBool
        _           -> throwError $ Mismatch t Syn.tyBool
    Syn.SetCompl e -> check e
  
  Syn.EBinOp op -> case op of
    Syn.Add e1 e2 -> checkBinaryOp Syn.tyInt e1 e2
    Syn.Sub e1 e2 -> checkBinaryOp Syn.tyInt e1 e2
    Syn.Mul e1 e2 -> checkBinaryOp Syn.tyInt e1 e2
    Syn.Div e1 e2 -> checkBinaryOp Syn.tyInt e1 e2
    Syn.Conj e1 e2 -> checkBinaryOp Syn.tyBool e1 e2
    Syn.Disj e1 e2 -> checkBinaryOp Syn.tyBool e1 e2
    Syn.Impl e1 e2 -> checkBinaryOp Syn.tyBool e1 e2
    Syn.Eq e1 e2 -> do
      t1 <- check e1
      t2 <- check e2
      case t1 of
        (Syn.TyIntP)  | t2 == Syn.tyInt  -> pure Syn.tyInt
        (Syn.TyBoolP) | t2 == Syn.tyBool -> pure Syn.tyBool
        _ -> throwError $ Mismatch t1 t2
    Syn.SetUnion e0 e1 -> checkSetEs e0 e1
    Syn.SetInter e0 e1 -> checkSetEs e0 e1
    Syn.SetDiff e0 e1 -> checkSetEs e0 e1
    Syn.SetSubS e0 e1 -> checkSetEs e0 e1 >> pure Syn.tyBool
    Syn.SetMem e0 e1 -> checkSetEs e0 e1 >> pure Syn.tyBool

  Syn.Pred n ns -> mapM_ check ns >> pure Syn.tyBool

  Syn.UnivQ n t e -> checkQuant n t e
  Syn.ExisQ n t e -> checkQuant n t e

  Syn.Lam n t e -> do
    bodyT <- inCtx (n,t) (check e)
    pure (Syn.TyFunc t bodyT)

  Syn.App e0 e1 -> do
    t0 <- check e0
    t1 <- check e1
    case t0 of
      (Syn.TyFunc a b) | a == t1   -> pure b
                       | otherwise -> throwError $ Mismatch t1 a
      t -> throwError $ NotFunction e0 e1
  
  Syn.ESet es -> do
    ts <- mapM check (Set.elems es)
    let init = head ts
    if (and $ map (== init) (tail ts))
      then pure init
      else throwError $ HeterogenousSet es

  where 
    checkSetEs e0 e1 = do
      t1 <- check e0
      t2 <- check e1
      if t1 == t2
        then pure t1
        else throwError $ Mismatch t1 t2

runCheck :: Ctx -> Check a -> Either TypeError a
runCheck ctx = flip runReader ctx . runExceptT

checkExpr :: Ctx -> Syn.Expr -> Either TypeError Syn.Type
checkExpr ctx x = runCheck ctx $ (check x)