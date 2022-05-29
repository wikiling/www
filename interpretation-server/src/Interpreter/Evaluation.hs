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
  | VFunc Syn.Expr

instance Show Value where
  show (VInt x) = show x
  show (VEnt x) = x
  show (VBool x) = show x
  show (VFunc exp) = show exp

type Evaluation = Identity Value

eval :: Syn.Expr -> Evaluation
eval expr = let

  guardBool e = case eval e of
    Identity (VBool b) -> b
  guardInt e = case eval e of
    Identity (VInt i) -> i
  evalArith op e0 e1 = pure $ VInt $ op (guardInt e0) (guardInt e1)

  in case expr of

    Syn.ELit (Syn.LInt x) -> pure $ VInt (fromIntegral x)
    Syn.ELit (Syn.LBool x) -> pure $ VBool x

    Syn.ESym (Syn.SVar x) _   -> error ("Unbound variable: " ++ show x)
    Syn.ESym (Syn.SConst x) _ -> pure $ VEnt x

    Syn.EUnOp op -> case op of
      Syn.Neg e -> negate e where
        negate = pure . VBool . not . guardBool

    Syn.EBinOp op -> case op of
      Syn.Add a b -> evalArith (+) a b
      Syn.Mul a b -> evalArith (*) a b
      Syn.Sub a b -> evalArith (-) a b
      Syn.Div a b -> evalArith (div) a b

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

    -- No model theory yet, so all predicates are true.
    Syn.Pred n ns -> pure $ VBool True
    -- Syn.UnivQ n _ e ->
    -- Syn.ExisQ n _ e ->
    -- Syn.IotaQ n _ e ->

    l@(Syn.Lam _ _ _) -> pure $ VFunc l

    -- cbn beta reduction. let's make this cbv once the Value type is more stable
    -- (λt:i → (λP:<v,t> → (∃e:v → T(e) & P(e)))) T <<v,t>,t> λP:<v,t> → (∃e:v → T(e) & P(e))
    -- ((λy:e → (λx:e → (λe:v → stab(e, y, x)))) Caesar) Brutus <v,t> λe:v → stab(e, Caesar, Brutus)
    -- ((\t:<i> . (\P:<v,t> . (exists e:<v> . Time(e) & P(e)))) T:i) (((\y:<e> . (\x:<e> . (\e:<v> . Stab(e,y,x)))) Caesar:e) Brutus:e)
    -- (\P:<v,t> . (exists e:<v> . Time(e) & P(e))) (((\y:<e> . (\x:<e> . (\e:<v> . Stab(e,y,x)))) Caesar:e) Brutus:e)
    -- (exists f:<v> . Time(f) & (((\y:<e> . (\x:<e> . (\e:<v> . Stab(e,y,x)))) Caesar:e) Brutus:e) f)
    --
    -- exists f:<v> . Time(f) & ((((\y:<e> . (\x:<e> . (\e:<v> . Stab(e,y,x)))) Caesar:e) Brutus:e) f)
    -- need evaluation of quantifiers!
    Syn.App e0 e1 -> case e0 of
      a@(Syn.App _ _) -> do
        lhs <- eval a
        case lhs of
          VFunc (Syn.Lam param _ body) -> eval $ Syn.substitute e1 param body
          nf -> error ("Tried to apply non fn: " ++ show nf ++ " to " ++ show e1)
      Syn.Lam param _ body -> eval $ Syn.substitute e1 param body
      _ -> error ("Tried to apply non fn: " ++ show e0 ++ " to " ++ show e1)

runEval :: Syn.Expr -> Value
runEval x = runIdentity (eval x)

