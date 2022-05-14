module Lang.Check (
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
lookupVar x = do
  env <- ask
  case lookup x env of
    Just e  -> return e
    Nothing -> throwError $ NotInScope x

checkArithmetic :: S.Expr -> S.Expr -> Check S.Type
checkArithmetic e1 e2 = do
  t1 <- check e1
  t2 <- check e2
  if t1 == S.TInt
    then if t2 == S.TInt
      then return S.TInt
      else throwError $  Mismatch t2 S.TInt
    else throwError $  Mismatch t1 S.TInt

check :: S.Expr -> Check S.Type
check expr = case expr of

  S.Lit S.LInt{} -> return S.TInt

  S.Lit S.LBool{} -> return S.TBool

  S.Var x -> lookupVar x

  S.Add e1 e2 -> checkArithmetic e1 e2
  S.Sub e1 e2 -> checkArithmetic e1 e2
  S.Mul e1 e2 -> checkArithmetic e1 e2
  S.Div e1 e2 -> checkArithmetic e1 e2

  S.Eq e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case t1 of
      (S.TInt) | t2 == S.TInt -> return S.TInt
      (S.TBool) | t2 == S.TBool -> return S.TBool
      _ -> throwError $ Mismatch t1 t2

  S.Lam x t e -> do
    body <- inEnv (x,t) (check e)
    return (S.TArr t body)

  S.App e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case t1 of
       (S.TArr a b) | a == t2 -> return b
                    | otherwise -> throwError $ Mismatch t2 a
       ty -> throwError $ NotFunction ty

runCheck :: Env -> Check a -> Either TypeError a
runCheck env = flip runReader env . runExceptT

checkTop :: Env -> S.Expr -> Either TypeError S.Type
checkTop env x = runCheck env $ (check x)