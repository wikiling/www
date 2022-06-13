{-# LANGUAGE PatternSynonyms #-}

module Interpreter.Evaluation where

import Compiler.Pretty
import qualified Compiler.Syntax as Syn

import Control.Monad.Identity
import qualified Data.Map as Map

import Debug.Trace (traceM)

data Value
  = VInt Integer
  | VBool Bool
  | VFormula Syn.Expr
  | VFunc Syn.Expr
  | VClosure Syn.Binder

instance Show Value where
  show (VInt x) = show x
  show (VFormula exp) = show exp
  show (VBool x) = show x
  show (VFunc exp) = show exp
  show (VClosure b) = show b

type Evaluation = Identity Value
type EvalCtx = Map.Map Syn.Name Value

eval :: EvalCtx -> Syn.Expr -> Evaluation
eval ctx expr = let

  guardBool e = case eval ctx e of
    Identity (VBool b) -> b
  guardInt e = case eval ctx e of
    Identity (VInt i) -> fromIntegral i
  guardForm ctx' e = case eval ctx' e of
    Identity (VFormula e') -> e'

  subformula :: Syn.UnOp -> Syn.Expr -> Evaluation
  subformula f e = pure $ VFormula $ Syn.EUnOp f $ guardForm ctx e

  subformulae :: ([Syn.Expr] -> Syn.Expr) -> [Syn.Expr] -> Evaluation
  subformulae f es = pure $ VFormula $ f (map (guardForm ctx) es)

  subformulae2 :: Syn.BinOp -> Syn.Expr -> Syn.Expr -> Evaluation
  subformulae2 f e0 e1 = pure $ VFormula $ Syn.EBinOp f (guardForm ctx e0) (guardForm ctx e1)

  arithFormula op e0 e1 = pure $ (VFormula . Syn.ELit . Syn.LInt) (op (guardInt e0) (guardInt e1))

  in case expr of
    l@(Syn.ELit (Syn.LInt _)) -> pure $ VFormula l
    l@(Syn.ELit (Syn.LBool _)) -> pure $ VFormula l

    -- if we've hit a variable then it has escaped
    -- type checking and beta reduction, in which case it
    -- must be bound by a quantifier
    Syn.Var n -> case Map.lookup n ctx of
      Nothing -> error ("Unbound variable: " ++ show n)
      Just v -> pure v

    c@(Syn.Const _ _) -> pure $ VFormula c

    {-
    Syn.EUnOp op -> case op of
      Syn.Neg e -> negate e where
        negate = pure . VBool . not . guardBool
    -}

    Syn.EUnOp q e -> subformula q e

    Syn.EBinOp op e0 e1 -> case op of
      Syn.Add -> arithFormula (+) e0 e1
      Syn.Mul -> arithFormula (*) e0 e1
      Syn.Sub -> arithFormula (-) e0 e1
      Syn.Div -> arithFormula (div) e0 e1
      _ -> subformulae2 op e0 e1

    Syn.Pred n es -> subformulae (Syn.Pred n) es

    Syn.EBinder b -> pure $ VClosure b

    Syn.EQuant q b@(Syn.Binder n t) e -> pure $ VFormula $ Syn.EQuant q b $ guardForm (Map.insert n (VFormula $ Syn.Var n) ctx) e

    l@Syn.Lam{} -> pure $ VFunc l

    -- | Cbn beta reduction. let's make this cbv once the Value type is more stable.
    --   We also construct Syn.Pred expressions out of applications of Syn.Const
    --   to expressions.
    Syn.App e0 e1 -> let
      nf e = error ("Tried to apply non fn: " ++ show e ++ " to " ++ show e1)
      betaReduce arg body = eval ctx $ Syn.substitute e1 arg body
      mkVFormPredicate c args = case eval ctx e1 of
        Identity (VFormula a@(Syn.Var s)) -> mk a
        Identity (VFormula a@(Syn.Const s t)) -> mk a
        e -> error ("Argument to predicate must evaluate to a symbol. Found: " ++ show e ++ " for predicate: " ++ c)
        where mk arg = VFormula $ Syn.Pred c (args ++ [arg])
      in case e0 of
        a@(Syn.App _ _) -> do
          lhs <- eval ctx a
          case lhs of
            VFunc (Syn.Lam (Syn.Binder n _) body) -> betaReduce n body
            VFormula (Syn.Pred n args) -> pure $ mkVFormPredicate n args
            e -> nf e
        Syn.Lam (Syn.Binder n _) body -> betaReduce n body
        Syn.Const c _ -> pure $ mkVFormPredicate c []
        e -> nf e

runEval :: Syn.Expr -> Value
runEval x = runIdentity (eval Map.empty x)

