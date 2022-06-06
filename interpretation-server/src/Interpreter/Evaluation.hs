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

instance Show Value where
  show (VInt x) = show x
  show (VFormula exp) = show exp
  show (VBool x) = show x
  show (VFunc exp) = show exp

type Evaluation = Identity Value
type EvalCtx = Map.Map Syn.Name Value

pattern Const n <- Syn.ESym (Syn.SConst n)

eval :: EvalCtx -> Syn.Expr -> Evaluation
eval ctx expr = let

  guardBool e = case eval ctx e of
    Identity (VBool b) -> b
  guardInt e = case eval ctx e of
    Identity (VInt i) -> fromIntegral i
  guardForm ctx' e = case eval ctx' e of
    Identity (VFormula e') -> e'

  subformula :: (Syn.Expr -> Syn.UnOp) -> Syn.Expr -> Evaluation
  subformula f e = pure $ VFormula $ Syn.EUnOp $ f $ guardForm ctx e

  subformulae :: ([Syn.Expr] -> Syn.Expr) -> [Syn.Expr] -> Evaluation
  subformulae f es = pure $ VFormula $ f (map (guardForm ctx) es)

  subformulae2 :: (Syn.Expr -> Syn.Expr -> Syn.BinOp) -> Syn.Expr -> Syn.Expr -> Evaluation
  subformulae2 f e0 e1 = pure $ VFormula $ Syn.EBinOp $ f (guardForm ctx e0) (guardForm ctx e1)

  qFormula f n t e = pure $ VFormula $ f n t $ guardForm (Map.insert n (VFormula $ Syn.ESym $ Syn.SVar n) ctx) e

  arithFormula op e0 e1 = pure $ (VFormula . Syn.ELit . Syn.LInt) (op (guardInt e0) (guardInt e1))

  in case expr of
    l@(Syn.ELit (Syn.LInt _)) -> pure $ VFormula l
    l@(Syn.ELit (Syn.LBool _)) -> pure $ VFormula l

    -- if we've hit a variable then it has escaped
    -- type checking and beta reduction, in which case it
    -- must be bound by a quantifier
    Syn.ESym (Syn.SVar s) -> case Map.lookup s ctx of
      Nothing -> error ("Unbound variable: " ++ show s)
      Just v -> pure v

    c@(Const _) -> pure $ VFormula c

    {-
    Syn.EUnOp op -> case op of
      Syn.Neg e -> negate e where
        negate = pure . VBool . not . guardBool
    -}

    Syn.EUnOp (Syn.Neg e) -> subformula Syn.Neg e

    Syn.EBinOp op -> case op of
      Syn.Add a b -> arithFormula (+) a b
      Syn.Mul a b -> arithFormula (*) a b
      Syn.Sub a b -> arithFormula (-) a b
      Syn.Div a b -> arithFormula (div) a b

      {-
      Syn.Eq a b -> do
        x <- eval a
        y <- eval b
        case x of
          VInt i1 -> case y of
            VInt i2 -> pure $ VBool (i1 == i2)
          VBool b1 -> case y of
            VBool b2 -> pure $ VBool (b1 == b2)
  
      Syn.Conj e0 e1 -> conjoin [e0,e1] where
        conjoin = pure . VBool . and . map guardBool
      Syn.Disj e0 e1 -> disjoin [e0,e1] where
        disjoin = pure . VBool . or . map guardBool
      Syn.Impl e0 e1 -> imply e0 e1 where
        imply e0 e1 = pure $ VBool $ not (and [(guardBool e0),(not $ guardBool e1)])
      -}
      Syn.Eq e0 e1 -> subformulae2 Syn.Eq e0 e1
      Syn.Conj e0 e1 -> subformulae2 Syn.Conj e0 e1
      Syn.Disj e0 e1 -> subformulae2 Syn.Disj e0 e1
      Syn.Impl e0 e1 -> subformulae2 Syn.Impl e0 e1

      Syn.SetUnion e0 e1 -> subformulae2 Syn.SetUnion e0 e1
      Syn.SetInter e0 e1 -> subformulae2 Syn.SetInter e0 e1
      Syn.SetDiff e0 e1 -> subformulae2 Syn.SetDiff e0 e1
      Syn.SetSubS e0 e1 -> subformulae2 Syn.SetSubS e0 e1
      Syn.SetMem e0 e1 -> subformulae2 Syn.SetMem e0 e1

    Syn.Pred n es -> subformulae (Syn.Pred n) es
    Syn.UnivQ n t e -> qFormula Syn.UnivQ n t e
    Syn.ExisQ n t e -> qFormula Syn.ExisQ n t e
    Syn.IotaQ n t e -> qFormula Syn.IotaQ n t e

    l@(Syn.Lam _ _ _) -> pure $ VFunc l

    -- cbn beta reduction. let's make this cbv once the Value type is more stable
    Syn.App e0 e1 -> let
      nf e = error ("Tried to apply non fn: " ++ show e ++ " to " ++ show e1)
      betaReduce arg body = eval ctx $ Syn.substitute e1 arg body
      mkVFormPredicate c args = case eval ctx e1 of
        Identity (VFormula (Syn.ESym s)) -> VFormula $ Syn.Pred c (args ++ [Syn.ESym s])
        e -> error ("Argument to predicate must evaluate to a symbol. Found: " ++ show e ++ " for predicate: " ++ c)
      in case e0 of
        a@(Syn.App _ _) -> do
          lhs <- eval ctx a
          case lhs of
            VFunc (Syn.Lam n _ body) -> betaReduce n body
            VFormula (Syn.Pred n args) -> pure $ mkVFormPredicate n args
            e -> nf e
        Syn.Lam n _ body -> betaReduce n body
        Const c -> pure $ mkVFormPredicate c []
        e -> nf e

runEval :: Syn.Expr -> Value
runEval x = runIdentity (eval Map.empty x)

