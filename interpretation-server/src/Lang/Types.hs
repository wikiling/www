module Lang.Types (
  check,
  checkTop,
  TypeError(..)
) where

import qualified Lang.Syntax as S
import Control.Monad.Except
import Control.Monad.Reader

type Env = [(S.Name, S.Type)]

extend :: (S.Name, S.Type) -> Env -> Env
extend xt env = xt : env

data TypeError
  = Mismatch S.Type S.Type
  | NotFunction S.Type
  | NotInScope S.Name

type Check = ExceptT TypeError (Reader Env)

inEnv :: (S.Name, S.Type) -> Check a -> Check a
inEnv (x,t) = local (extend (x,t))

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
    bodyT <- inEnv (n,t) (check e)
    if bodyT == S.TBool
      then pure S.TBool
      else throwError $ Mismatch bodyT S.TBool

check :: S.Expr -> Check S.Type
check expr = case expr of
  S.Lit S.LInt{} -> pure S.TInt
  S.Lit S.LBool{} -> pure S.TBool
  S.Lit S.LConst{} -> pure S.TEnt

  S.Var x -> lookupVar x

  S.Add e1 e2 -> checkBinaryOp S.TInt e1 e2
  S.Sub e1 e2 -> checkBinaryOp S.TInt e1 e2
  S.Mul e1 e2 -> checkBinaryOp S.TInt e1 e2
  S.Div e1 e2 -> checkBinaryOp S.TInt e1 e2

  S.Pred n ns -> mapM_ lookupVar ns >> pure S.TBool

  S.Neg e -> do
    t <- check e
    case t of
      S.TBool -> pure S.TBool
      _       -> throwError $ Mismatch t S.TBool

  S.Conj e1 e2 -> checkBinaryOp S.TBool e1 e2
  S.Disj e1 e2 -> checkBinaryOp S.TBool e1 e2
  S.Impl e1 e2 -> checkBinaryOp S.TBool e1 e2
  S.UnivQ n t e -> checkQuant n t e
  S.ExisQ n t e -> checkQuant n t e

  S.Eq e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case t1 of
      (S.TInt)  | t2 == S.TInt  -> pure S.TInt
      (S.TBool) | t2 == S.TBool -> pure S.TBool
      _                         -> throwError $ Mismatch t1 t2

  S.Lam n t e -> do
    bodyT <- inEnv (n,t) (check e)
    pure (S.TFunc t bodyT)

  S.App e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case t1 of
      (S.TFunc a b) | a == t2   -> pure b
                    | otherwise -> throwError $ Mismatch t2 a
      ty -> throwError $ NotFunction ty

runCheck :: Env -> Check a -> Either TypeError a
runCheck env = flip runReader env . runExceptT

checkTop :: Env -> S.Expr -> Either TypeError S.Type
checkTop env x = runCheck env $ (check x)