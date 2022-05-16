module Lang.Sem where

import Lang.Pretty
import qualified Lang.Syn as S

import Control.Monad.Identity
import qualified Data.Map as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String S.Expr (EvalCtx)

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show (VClosure n exp _) = show exp

type Evaluate t = Identity t
type EvalCtx = Map.Map String Value

eval :: EvalCtx -> S.Expr -> Identity Value
eval env expr = let

  guardBool e = case eval env e of
    Identity (VBool b) -> b
  guardInt e = case eval env e of
    Identity (VInt i) -> i
  evalArith op e1 e2 = pure $ VInt $ op (guardInt e1) (guardInt e2)

  in case expr of

    S.ELit (S.LInt x) -> pure $ VInt (fromIntegral x)
    S.ELit (S.LBool x) -> pure $ VBool x

    S.ETerm (S.TVar x) -> pure $ env Map.! x
    S.ETerm (S.TConst x) -> pure $ env Map.! x

    S.Add a b -> evalArith (+) a b
    S.Mul a b -> evalArith (*) a b
    S.Sub a b -> evalArith (-) a b
    S.Div a b -> evalArith (div) a b

    S.Eq a b -> do
      x <- eval env a
      y <- eval env b
      case x of
        VInt i1 -> case y of
          VInt i2 -> pure $ VBool (i1 == i2)
        VBool b1 -> case y of
          VBool b2 -> pure $ VBool (b1 == b2)

    -- No model theory yet, so all predicates are true.
    S.Pred n ns -> pure $ VBool True
    S.Neg e -> negate e where
      negate = pure . VBool . not . guardBool
    S.Conj e1 e2 -> conjoin [e1,e2] where
      conjoin = pure . VBool . and . map guardBool
    S.Disj e1 e2 -> disjoin [e1,e2] where
      disjoin = pure . VBool . or . map guardBool
    S.Impl e1 e2 -> imply e1 e2 where
      imply e1 e2 = pure $ VBool $ not (and [(guardBool e1), (not $ guardBool e2)])
    -- S.UnivQ n _ e ->
    -- S.ExisQ n _ e ->
    -- S.IotaQ n _ e ->

    S.Lam x _ e -> pure (VClosure x e env)

    S.App a b -> do
      x <- eval env a
      y <- eval env b
      apply x y

extend :: EvalCtx -> String -> Value -> EvalCtx
extend env v t = Map.insert v t env

apply :: Value -> Value -> Evaluate Value
apply (VClosure v t0 e) t1 = eval (extend e v t1) t0
apply _ _  = error "Tried to apply closure"

emptyScope :: EvalCtx
emptyScope = Map.empty

runEval :: S.Expr -> Value
runEval x = runIdentity (eval emptyScope x)