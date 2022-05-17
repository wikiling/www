module Compile.Type (
  check,
  checkTop,
  TypeError(..)
) where

import qualified Compile.Syn as S

import Control.Monad.Except
import Control.Monad.Reader

import Debug.Trace (traceM)

type Ctx = [(S.Name, S.Type)]

extend :: (S.Name, S.Type) -> Ctx -> Ctx
extend xt env = xt : env

data TypeError
  = Mismatch S.Type S.Type
  | NotFunction S.Type
  | NotInScope S.Name

type Check = ExceptT TypeError (Reader Ctx)

inCtx :: (S.Name, S.Type) -> Check a -> Check a
inCtx (x,t) = local (extend (x,t))

lookupVar :: S.Name -> Check S.Type
lookupVar n = do
  env <- ask
  case lookup n env of
    Just e  -> pure e
    Nothing -> throwError $ NotInScope n

lookupVars :: [S.Name] -> [Check S.Type]
lookupVars ns = lookupVar <$> ns

checkBinaryOp :: S.Type -> S.Expr -> S.Expr -> Check S.Type
checkBinaryOp t e1 e2 = do
  t1 <- check e1
  t2 <- check e2
  if t1 == t
    then if t2 == t
      then pure t
      else throwError $  Mismatch t2 t
    else throwError $  Mismatch t1 t

checkQuant :: S.Name -> S.Type -> S.Expr -> Check S.Type 
checkQuant n t e = do
    bodyT <- inCtx (n,t) (check e)
    if bodyT == S.TyBool
      then pure S.TyBool
      else throwError $ Mismatch bodyT S.TyBool

checkTerm :: S.Sym -> Check S.Type
checkTerm t = case t of
  S.SVar v   -> lookupVar v
  S.SConst c -> pure S.TyEnt

check :: S.Expr -> Check S.Type
check expr = case expr of
  S.ELit S.LInt{} -> pure S.TyInt
  S.ELit S.LBool{} -> pure S.TyBool

  S.ESym t -> checkTerm t

  S.Add e1 e2 -> checkBinaryOp S.TyInt e1 e2
  S.Sub e1 e2 -> checkBinaryOp S.TyInt e1 e2
  S.Mul e1 e2 -> checkBinaryOp S.TyInt e1 e2
  S.Div e1 e2 -> checkBinaryOp S.TyInt e1 e2

  S.Pred n ns -> mapM_ checkTerm ns >> pure S.TyBool

  S.Neg e -> do
    t <- check e
    case t of
      S.TyBool -> pure S.TyBool
      _        -> throwError $ Mismatch t S.TyBool

  S.Conj e1 e2 -> checkBinaryOp S.TyBool e1 e2
  S.Disj e1 e2 -> checkBinaryOp S.TyBool e1 e2
  S.Impl e1 e2 -> checkBinaryOp S.TyBool e1 e2
  S.UnivQ n t e -> checkQuant n t e
  S.ExisQ n t e -> checkQuant n t e

  S.Eq e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case t1 of
      (S.TyInt)  | t2 == S.TyInt  -> pure S.TyInt
      (S.TyBool) | t2 == S.TyBool -> pure S.TyBool
      _                           -> throwError $ Mismatch t1 t2

  -- TODO: if t is TyFunc check that its domain matches the type of e
  S.Lam n t e -> do
    bodyT <- inCtx (n,t) (check e)
    pure (S.TyFunc t bodyT)

  S.App e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case t1 of
      (S.TyFunc a b) | a == t2   -> pure b
                     | otherwise -> throwError $ Mismatch t2 a
      t -> throwError $ NotFunction t

runCheck :: Ctx -> Check a -> Either TypeError a
runCheck env = flip runReader env . runExceptT

checkTop :: Ctx -> S.Expr -> Either TypeError S.Type
checkTop env x = runCheck env $ (check x)