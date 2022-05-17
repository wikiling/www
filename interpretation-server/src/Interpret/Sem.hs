module Interpret.Sem where

import Compile.Pretty
import qualified Compile.Syn as S

import Control.Monad.Identity
import qualified Data.Map as Map

import Debug.Trace (traceM)

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String S.Expr EvalCtx

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show (VClosure n exp _) = show exp

type Evaluate t = Identity t
type EvalCtx = Map.Map String Value

eval :: EvalCtx -> S.Expr -> Identity Value
eval ctx expr = let

  guardBool e = case eval ctx e of
    Identity (VBool b) -> b
  guardInt e = case eval ctx e of
    Identity (VInt i) -> i
  evalArith op e1 e2 = pure $ VInt $ op (guardInt e1) (guardInt e2)

  in case expr of

    S.ELit (S.LInt x) -> pure $ VInt (fromIntegral x)
    S.ELit (S.LBool x) -> pure $ VBool x

    S.ESym (S.SVar x) -> pure $ ctx Map.! x
    S.ESym (S.SConst x) -> pure $ ctx Map.! x

    S.Add a b -> evalArith (+) a b
    S.Mul a b -> evalArith (*) a b
    S.Sub a b -> evalArith (-) a b
    S.Div a b -> evalArith (div) a b

    S.Eq a b -> do
      x <- eval ctx a
      y <- eval ctx b
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
      imply e1 e2 = pure $ VBool $ not (and [(guardBool e1),(not $ guardBool e2)])
    -- S.UnivQ n _ e ->
    -- S.ExisQ n _ e ->
    -- S.IotaQ n _ e ->

    S.Lam x _ e -> pure (VClosure x e ctx)

    S.App a b -> do
      traceM "eval 1..."
      x <- eval ctx a
      traceM "eval 2..."
      y <- eval ctx b
      traceM "eval 3..."
      apply x y

extend :: EvalCtx -> String -> Value -> EvalCtx
extend ctx v t = Map.insert v t ctx

apply :: Value -> Value -> Evaluate Value
apply (VClosure n body ctx) arg = eval (extend ctx n arg) body
apply _ _  = error "Tried to apply closure"

emptyCtx :: EvalCtx
emptyCtx = Map.empty

runEval :: S.Expr -> Value
runEval x = runIdentity (eval emptyCtx x)