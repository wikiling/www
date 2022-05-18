module Compiler.Types (
  check,
  checkTop,
  TypeError(..)
) where

import qualified Compiler.Syntax as Syn

import Control.Monad.Except
import Control.Monad.Reader

import Debug.Trace (traceM)

type Ctx = [(Syn.Name, Syn.Type)]

extend :: (Syn.Name, Syn.Type) -> Ctx -> Ctx
extend xt ctx = xt : ctx

data TypeError
  = Mismatch Syn.Type Syn.Type
  | NotFunction Syn.Type
  | NotInScope Syn.Name

type Check = ExceptT TypeError (Reader Ctx)

inCtx :: (Syn.Name, Syn.Type) -> Check a -> Check a
inCtx (x,t) = local (extend (x,t))

lookupVar :: Syn.Name -> Check Syn.Type
lookupVar n = do
  ctx <- ask
  case lookup n ctx of
    Just e  -> pure e
    Nothing -> throwError $ NotInScope n

lookupVars :: [Syn.Name] -> [Check Syn.Type]
lookupVars ns = lookupVar <$> ns

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
    if bodyT == Syn.TyBool
      then pure Syn.TyBool
      else throwError $ Mismatch bodyT Syn.TyBool

checkTerm :: Syn.Sym -> Check Syn.Type
checkTerm t = case t of
  Syn.SVar v   -> lookupVar v
  Syn.SConst c -> pure Syn.TyEnt

check :: Syn.Expr -> Check Syn.Type
check expr = case expr of
  Syn.ELit Syn.LInt{} -> pure Syn.TyInt
  Syn.ELit Syn.LBool{} -> pure Syn.TyBool

  Syn.ESym t -> checkTerm t

  Syn.Add e1 e2 -> checkBinaryOp Syn.TyInt e1 e2
  Syn.Sub e1 e2 -> checkBinaryOp Syn.TyInt e1 e2
  Syn.Mul e1 e2 -> checkBinaryOp Syn.TyInt e1 e2
  Syn.Div e1 e2 -> checkBinaryOp Syn.TyInt e1 e2

  Syn.Pred n ns -> mapM_ checkTerm ns >> pure Syn.TyBool

  Syn.Neg e -> do
    t <- check e
    case t of
      Syn.TyBool -> pure Syn.TyBool
      _        -> throwError $ Mismatch t Syn.TyBool

  Syn.Conj e1 e2 -> checkBinaryOp Syn.TyBool e1 e2
  Syn.Disj e1 e2 -> checkBinaryOp Syn.TyBool e1 e2
  Syn.Impl e1 e2 -> checkBinaryOp Syn.TyBool e1 e2
  Syn.UnivQ n t e -> checkQuant n t e
  Syn.ExisQ n t e -> checkQuant n t e

  Syn.Eq e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case t1 of
      (Syn.TyInt)  | t2 == Syn.TyInt  -> pure Syn.TyInt
      (Syn.TyBool) | t2 == Syn.TyBool -> pure Syn.TyBool
      _                           -> throwError $ Mismatch t1 t2

  -- TODO: if t is TyFunc check that its domain matches the type of e
  Syn.Lam n t e -> do
    bodyT <- inCtx (n,t) (check e)
    pure (Syn.TyFunc t bodyT)

  Syn.App e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case t1 of
      (Syn.TyFunc a b) | a == t2   -> pure b
                     | otherwise -> throwError $ Mismatch t2 a
      t -> throwError $ NotFunction t

runCheck :: Ctx -> Check a -> Either TypeError a
runCheck ctx = flip runReader ctx . runExceptT

checkTop :: Ctx -> Syn.Expr -> Either TypeError Syn.Type
checkTop ctx x = runCheck ctx $ (check x)