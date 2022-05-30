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
  subformulae f es = pure $ VFormula $ f (map guardForm es)

  subformulae2 :: (Syn.Expr -> Syn.Expr -> Syn.BinOp) -> Syn.Expr -> Syn.Expr -> Evaluation
  subformulae2 f e0 e1 = pure $ VFormula $ Syn.EBinOp $ f (guardForm ctx e0) (guardForm ctx e1)

  qFormula f e0 e1 = case e0 of
    s@(Syn.ESym (Syn.SVar n) t) ->
      pure $ VFormula $ f n t $ guardForm (Map.insert n (VFormula s) ctx) e1

  arithFormula op e0 e1 = pure $ (VFormula . Syn.ELit . Syn.LInt) (op (guardInt e0) (guardInt e1))

  in case expr of
    l@(Syn.ELit (Syn.LInt _)) -> pure $ VFormula l
    l@(Syn.ELit (Syn.LBool _)) -> pure $ VFormula l

    -- if we've hit a variable then it has passed through
    -- type checking and beta reduction, in which case it
    -- must be bound by a quantifier
    Syn.ESym (Syn.SVar s) _ -> case Map.lookup s ctx of
      Nothing -> error ("Unbound variable: " ++ show s)
      Just v -> pure v

    c@(Syn.ESym (Syn.SConst _) _) -> pure $ VFormula c

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

    Syn.Pred n es -> subformulae (Syn.Pred n) es
    Syn.UnivQ e0 e1 -> qFormula Syn.UnivQ e0 e1
    Syn.ExisQ e0 e1 -> qFormula Syn.ExisQ e0 e1
    Syn.IotaQ e0 e1 -> qFormula Syn.IotaQ e0 e1

    l@(Syn.Lam _ _ _) -> pure $ VFunc l

    -- cbn beta reduction. let's make this cbv once the Value type is more stable
    -- (λt:i → (λP:<v,t> → (∃e:v → T(e) & P(e)))) T <<v,t>,t> λP:<v,t> → (∃e:v → T(e) & P(e))
    -- ((λy:e → (λx:e → (λe:v → stab(e, y, x)))) Caesar) Brutus <v,t> λe:v → stab(e, Caesar,  jkxccBrutus)
    -- ((\t:<i> . (\P:<v,t> . (exists e:<v> . Time(e) & P(e)))) T:i) (((\y:<e> . (\x:<e> . (\e:<v> . Stab(e,y,x)))) Caesar:e) Brutus:e)
    -- (\P:<v,t> . (exists e:<v> . Time(e) & P(e))) (((\y:<e> . (\x:<e> . (\e:<v> . Stab(e,y,x)))) Caesar:e) Brutus:e)
    -- (exists f:<v> . Time(f) & (((\y:<e> . (\x:<e> . (\e:<v> . Stab(e,y,x)))) Caesar:e) Brutus:e) f)
    --
    -- exists f:<v> . Time(f) & ((((\y:<e> . (\x:<e> . (\e:<v> . Stab(e,y,x)))) Caesar:e) Brutus:e) f)
    -- need evaluation of quantifiers!
    Syn.App e0 e1 -> case e0 of
      a@(Syn.App _ _) -> do
        lhs <- eval ctx a
        case lhs of
          VFunc (Syn.Lam param _ body) -> eval ctx $ Syn.substitute e1 param body
          nf -> error ("Tried to apply non fn: " ++ show nf ++ " to " ++ show e1)
      Syn.Lam param _ body -> eval ctx $ Syn.substitute e1 param body
      _ -> error ("Tried to apply non fn: " ++ show e0 ++ " to " ++ show e1)

runEval :: Syn.Expr -> Value
runEval x = runIdentity (eval Map.empty x)

