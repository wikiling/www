module Lang.Eval where

import qualified Lang.Syntax as S

import Control.Monad.Identity
import qualified Data.Map as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String S.Expr (EScope)

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show VClosure{} = "<<closure>>"

type Evaluate t = Identity t
type EScope = Map.Map String Value

type ArithOp = Integer -> Integer -> Integer
evalArith :: EScope -> ArithOp -> S.Expr -> S.Expr -> Integer
evalArith env op e1 e2 = op (guardInt env e1) (guardInt env e2) where
  guardInt env expr = case eval env expr of
    Identity (VInt i) -> i

eval :: EScope -> S.Expr -> Identity Value
eval env expr = case expr of

  S.Lit (S.LInt x) -> return $ VInt (fromIntegral x)

  S.Lit (S.LBool x) -> return $ VBool x

  S.Var x -> return $ env Map.! x

  S.Add a b -> return $ VInt $ evalArith env (+) a b
  S.Mul a b -> return $ VInt $ evalArith env (*) a b
  S.Sub a b -> return $ VInt $ evalArith env (-) a b
  S.Div a b -> return $ VInt $ evalArith env (div) a b

  S.Eq a b -> do
    x <- eval env a
    y <- eval env b
    case x of
      VInt i1 -> case y of
        VInt i2 -> return $ VBool (i1 == i2)
      VBool b1 -> case y of
        VBool b2 -> return $ VBool (b1 == b2)

  | S.Pred n ns ->
  | S.Neg e ->
  | S.Conj e1 e2 ->
  | S.Disj e1 e2 ->
  | S.Impl e1 e2 ->
  | S.UnivQ n _ e ->
  | S.ExisQ n _ e ->

  S.Lam x _ e -> return (VClosure x e env)

  S.App a b -> do
    x <- eval env a
    y <- eval env b
    apply x y

extend :: EScope -> String -> Value -> EScope
extend env v t = Map.insert v t env

apply :: Value -> Value -> Evaluate Value
apply (VClosure v t0 e) t1 = eval (extend e v t1) t0
apply _ _  = error "Tried to apply closure"

emptyScope :: EScope
emptyScope = Map.empty

runEval :: S.Expr -> Value
runEval x = runIdentity (eval emptyScope x)