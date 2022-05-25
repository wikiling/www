module Interpreter.Evaluation where

import Compiler.Pretty
import qualified Compiler.Syntax as Syn

import Control.Monad.Identity
import qualified Data.Map as Map

import Debug.Trace (traceM)

data Value
  = VInt Integer
  | VEnt String
  | VBool Bool
  | VClosure String Syn.Expr EvalCtx

instance Show Value where
  show (VInt x) = show x
  show (VEnt x) = show x
  show (VBool x) = show x
  show (VClosure n exp _) = show exp

type Evaluation t = Identity t
type EvalCtx = Map.Map String Value

eval :: EvalCtx -> Syn.Expr -> Identity Value
eval ctx expr = let

  guardBool e = case eval ctx e of
    Identity (VBool b) -> b
  guardInt e = case eval ctx e of
    Identity (VInt i) -> i
  evalArith op e1 e2 = pure $ VInt $ op (guardInt e1) (guardInt e2)

  in case expr of

    Syn.ELit (Syn.LInt x) -> pure $ VInt (fromIntegral x)
    Syn.ELit (Syn.LBool x) -> pure $ VBool x

    Syn.ESym (Syn.SVar x) -> traceM x >> pure $ ctx Map.! x
    Syn.ESym (Syn.SConst x) -> traceM x >> pure $ ctx Map.! x

    Syn.Add a b -> evalArith (+) a b
    Syn.Mul a b -> evalArith (*) a b
    Syn.Sub a b -> evalArith (-) a b
    Syn.Div a b -> evalArith (div) a b

    Syn.Eq a b -> do
      x <- eval ctx a
      y <- eval ctx b
      case x of
        VInt i1 -> case y of
          VInt i2 -> pure $ VBool (i1 == i2)
        VBool b1 -> case y of
          VBool b2 -> pure $ VBool (b1 == b2)

    -- No model theory yet, so all predicates are true.
    Syn.Pred n ns -> pure $ VBool True
    Syn.Neg e -> negate e where
      negate = pure . VBool . not . guardBool
    Syn.Conj e1 e2 -> conjoin [e1,e2] where
      conjoin = pure . VBool . and . map guardBool
    Syn.Disj e1 e2 -> disjoin [e1,e2] where
      disjoin = pure . VBool . or . map guardBool
    Syn.Impl e1 e2 -> imply e1 e2 where
      imply e1 e2 = pure $ VBool $ not (and [(guardBool e1),(not $ guardBool e2)])
    -- Syn.UnivQ n _ e ->
    -- Syn.ExisQ n _ e ->
    -- Syn.IotaQ n _ e ->

    Syn.Lam x _ e -> do
      traceM "??"
      pure $ VClosure x e ctx

    Syn.App a b -> do
      traceM "??"
      x <- eval ctx a
      y <- eval ctx b
      traceM $ show y
      apply x y

extend :: EvalCtx -> String -> Value -> EvalCtx
extend ctx v t = Map.insert v t ctx

apply :: Value -> Value -> Evaluation Value
apply (VClosure n body ctx) arg = eval (extend ctx n arg) body
apply _ _  = error "Tried to apply non fn"

emptyCtx :: EvalCtx
emptyCtx = Map.empty

runEvalIn :: Syn.Expr -> EvalCtx -> Value
runEvalIn x ctx = runIdentity (eval ctx x)

runEval :: Syn.Expr -> Value
runEval x = runEvalIn x emptyCtx
