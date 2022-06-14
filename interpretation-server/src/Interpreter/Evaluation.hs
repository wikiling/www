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
  | VBoundVar Syn.Expr

instance Show Value where
  show (VInt x) = show x
  show (VFormula exp) = show exp
  show (VBool x) = show x
  show (VFunc exp) = show exp
  show (VBoundVar b) = show b

data EvalError
  = UnboundVariable Syn.Expr
  | NotAFn Syn.Expr Syn.Expr
  | PredArgMismatch Syn.Name Syn.Expr

instance Show EvalError where
  show (UnboundVariable e) = "Unbound variable: " ++ show e
  show (NotAFn e0 e1) = "Tried to apply non fn: " ++ show e0 ++ " to " ++ show e1
  show (PredArgMismatch n e) = "Argument to predicate must evaluate to a symbol. Found: " ++ show e ++ " for predicate: " ++ n

type Evaluation = ExceptT EvalError Identity
type EvalCtx = Map.Map Syn.Name Value

-- | Construct Syn.Pred expressions out of applications of Syn.Const
--   to expressions.
resolvePredicates :: Syn.Expr -> Syn.Expr
resolvePredicates = resolve
  where
    resolve e = case e of
      Syn.App e0 e1 -> let r1 = resolve e1 in case e0 of
        Syn.Const c _ -> Syn.Pred c [r1]
        _ -> let r0 = resolve e0 in case r0 of
          Syn.Pred n args -> Syn.Pred n (args ++ [r1])
          _ -> r0
      _ -> e

eval :: EvalCtx -> Syn.Expr -> Evaluation Value
eval ctx expr = let

  guardBool e = case runEvalIn ctx e of
    Left err -> throwError err
    Right (VBool b) -> pure b
  guardInt e = case runEvalIn ctx e of
    Left err -> throwError err
    Right (VInt i) -> pure $ fromIntegral i
  guardForm ctx' e = case runEvalIn ctx' e of
    Left err -> throwError err
    Right (VFormula e') -> pure e'

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

    -- if we've hit a variable then it has (a) escaped
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

    Syn.Pred n es -> subformulae (Syn.Pred n) es

    Syn.EBinder (Syn.Binder n _) -> pure $ VBoundVar $ Syn.Var n

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
        betaReduce arg body = eval ctx $ Syn.substitute e1 arg body

extendCtx :: Syn.Name -> Value -> EvalCtx -> EvalCtx
extendCtx = Map.insert

empyCtx = Map.empty

runEvalIn :: EvalCtx -> Syn.Expr -> Either EvalError Value
runEvalIn ctx e = runIdentity $ runExceptT $ eval ctx e

runEval :: Syn.Expr -> Either EvalError Value
runEval = runEvalIn empyCtx

