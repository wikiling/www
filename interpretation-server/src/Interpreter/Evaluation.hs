{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Interpreter.Evaluation where

import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map

import Compiler.Pretty
import qualified Compiler.Syntax as Syn

import Debug.Trace (traceM)

data Value
  = VInt Integer
  | VBool Bool
  | VFormula Syn.Expr
  | VFunc Syn.Expr
  -- supports AST operations on frontend via composition
  | VBinder Syn.Binder

instance Show Value where
  show (VInt x) = show x
  show (VFormula exp) = show exp
  show (VBool x) = show x
  show (VFunc exp) = show exp
  show (VBinder b) = "λ" ++ show b

data EvalError
  = UnboundVariable Syn.Expr
  | NotAFn Syn.Expr Syn.Expr
  | PredArgMismatch Syn.Name Syn.Expr
  | NotAFormula Value

instance Show EvalError where
  show (UnboundVariable e) = "Unbound variable: " ++ show e
  show (NotAFn e0 e1) = "Tried to apply non fn: " ++ show e0 ++ " to " ++ show e1
  show (PredArgMismatch n e) = "Argument to predicate must evaluate to a symbol. Found: " ++ show e ++ " for predicate: " ++ n
  show (NotAFormula v) = "Expecting a formula. Got: " ++ show v

type Evaluation = ExceptT EvalError Identity
type EvalCtx = Map.Map Syn.Name Value

eval :: EvalCtx -> Syn.Expr -> Evaluation Value
eval ctx expr = let

  guardBool e = eval ctx e >>= \v -> case v of
    VBool b -> pure b
  guardInt e = eval ctx e >>= \v -> case v of
    VInt i -> pure $ fromIntegral i
  guardForm ctx' e = eval ctx' e >>= \v -> case v of
    VFormula e' -> pure e'
    v -> throwError $ NotAFormula v

  subformula :: Syn.UnOp -> Syn.Expr -> Evaluation Value
  subformula f e = guardForm ctx e >>= pure . VFormula . (Syn.EUnOp f)

  subformulae :: ([Syn.Expr] -> Syn.Expr) -> [Syn.Expr] -> Evaluation Value
  subformulae f es = mapM (guardForm ctx) es >>= pure . VFormula . f 

  subformulae2 :: Syn.BinOp -> Syn.Expr -> Syn.Expr -> Evaluation Value
  subformulae2 op e0 e1 = do
    f0 <- (guardForm ctx e0)
    f1 <- (guardForm ctx e1)
    pure $ VFormula $ Syn.EBinOp op f0 f1

  arithFormula op e0 e1 = do
    i0 <- guardInt e0
    i1 <- guardInt e1
    pure $ (VFormula . Syn.ELit . Syn.LInt) (op i0 i1)

  in case expr of
    l@(Syn.ELit (Syn.LInt _)) -> pure $ VFormula l
    l@(Syn.ELit (Syn.LBool _)) -> pure $ VFormula l

    -- if we've hit a variable then it has (a) passed through
    -- type checking and beta reduction, in which case it
    -- must be bound by a quantifier, or (b) been passed
    -- in via ctx during composition
    Syn.Var n -> case Map.lookup n ctx of
      Nothing -> throwError $ UnboundVariable expr
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

    Syn.Pred n t es -> subformulae (Syn.Pred n t) es

    Syn.EBinder b -> pure $ VBinder b

    Syn.EQuant q b@(Syn.Binder n t) e -> do
      e' <- guardForm (Map.insert n (VFormula $ Syn.Var n) ctx) e
      pure $ VFormula $ Syn.EQuant q b e'

    l@Syn.Lam{} -> pure $ VFunc l

    -- | Cbn beta reduction. let's make this cbv once the Value type is more stable.
    Syn.App e0 e1 -> case e0 of
      a@(Syn.App _ _) -> do
        lhs <- eval ctx a
        case lhs of
          VFunc (Syn.Lam (Syn.Binder n _) body) -> betaReduce n body
          e -> nf
      Syn.Lam (Syn.Binder n _) body -> betaReduce n body
      e -> nf
      where
        nf = throwError $ NotAFn e0 e1
        betaReduce arg body = eval ctx $ Syn.resolvePredicates $ Syn.substitute e1 arg body

extendCtx :: Syn.Name -> Value -> EvalCtx -> EvalCtx
extendCtx = Map.insert

emptyCtx = Map.empty

runEvalIn :: EvalCtx -> Syn.Expr -> Either EvalError Value
runEvalIn ctx e = runIdentity $ runExceptT $ eval ctx e

runEval :: Syn.Expr -> Either EvalError Value
runEval = runEvalIn emptyCtx

