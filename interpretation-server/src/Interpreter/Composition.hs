{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}

module Interpreter.Composition (
  Tree(..),
  ConstituencyTree,
  ConstituencyNodeLabel(..),
  SemanticTree,
  SemanticNodeLabel(..),
  runComposition,
  printTree
) where

import GHC.Generics
import qualified Data.Map as Map
import Data.Tree.Binary.Preorder (Tree(..), printTree)
import Control.Monad.Identity
import Control.Monad.Reader

import Compiler.Pretty
import qualified Compiler.Syntax as Syn
import qualified Compiler.Parser as Parse
import qualified Compiler.Inference as Inf
import qualified Compiler.TypeEnv as TyEnv
import qualified Interpreter.Fragment as Frag
import qualified Interpreter.Evaluation as Sem
import qualified Lib.Std as Stdlib

import Debug.Trace (trace, traceM)

type Pos = String

data ConstituencyNodeLabel = CNodeLabel String Pos deriving (Show, Generic)

data SemanticNodeLabel
  = EvaluatedSemNode Syn.Expr Syn.Type Sem.Value ConstituencyNodeLabel
  | EmptySemNode ConstituencyNodeLabel

type ConstituencyTree = Tree ConstituencyNodeLabel
type SemanticTree = Tree SemanticNodeLabel

type FragmentCtx = Reader Frag.Fragment
type CompositionTree = FragmentCtx SemanticTree

instance Show SemanticNodeLabel where
  show (EvaluatedSemNode expr ty v _) = show expr ++ " " ++ show ty ++ " " ++ show v
  show (EmptySemNode _) = "<empty>"

checkLexicon :: Syn.Name -> FragmentCtx (Maybe Frag.LexicalEntry)
checkLexicon name = asks (Map.lookup name)

semNode :: Syn.Expr -> Syn.Type -> Sem.Value -> ConstituencyNodeLabel -> (SemanticTree -> SemanticTree -> SemanticTree)
semNode e t v cnl = Node (EvaluatedSemNode e t v cnl)

leafConstNode :: ConstituencyNodeLabel -> SemanticTree
leafConstNode cnl@(CNodeLabel n _) = (semNode c t (Sem.VFormula c) cnl) Leaf Leaf
  where
    t = Syn.TyVar $ Syn.TV "B"
    c = Syn.Const n t

pattern FnNode tDom tRan e <- Node (EvaluatedSemNode e (Syn.TyFun tDom tRan) _ _) _ _
pattern ArgNode t e <- Node (EvaluatedSemNode e t _ _) _ _

functionApp :: ConstituencyNodeLabel -> SemanticTree -> SemanticTree -> SemanticTree
functionApp cnl b1 b2 = case b1 of
  FnNode t1Dom t1Ran e1 -> case b2 of
    ArgNode t2 e2 | Inf.unifiable t1Dom t2 -> appNode e1 e2 t1Ran
    _ -> noAppNode
  ArgNode t1 e1 -> case b2 of
    FnNode t2Dom t2Ran e2 | Inf.unifiable t2Dom t1 -> appNode e2 e1 t2Ran
    _ -> noAppNode
  _ -> noAppNode
  where
    appNode :: Syn.Expr -> Syn.Expr -> Syn.Type -> SemanticTree
    appNode e1 e2 t0 = let
      e = (Syn.App e1 e2)
      t = case Inf.inferExpr TyEnv.empty e of
        Right (Syn.Forall _ ty) -> ty
        Left e -> Syn.TyCon $ show e
      in
        semNode e t (Sem.runEval e) cnl b1 b2

    noAppNode = Node (EmptySemNode cnl) b1 b2

compose :: ConstituencyTree -> CompositionTree
compose (Node cnl@(CNodeLabel label _) c1 c2) = case (c1,c2) of
  (Leaf, Leaf) -> case label of  -- terminal
    "" -> pure $ (semNode Stdlib.idFn Stdlib.idTy (Sem.runEval Stdlib.idFn) cnl) Leaf Leaf
    _ -> do
      traceM $ "checking ... " ++ label
      lex <- checkLexicon label
      traceM $ "found ... " ++ show lex
      case lex of
        Nothing -> pure $ leafConstNode cnl
        Just (expr, ty) -> pure $ (semNode expr ty (Sem.runEval expr) cnl) Leaf Leaf
  (_, Leaf) -> preTerm cnl c1    -- one child
  (Leaf, _) -> preTerm cnl c2
  _ -> do                        -- two children
    s1 <- compose c1
    s2 <- compose c2
    pure $ functionApp cnl s1 s2
  where
    -- replace any const C in a preterminal declaration [C] with the label of its child
    -- allows grammatical entries to be written in the form:
    -- `[C] = \x . C x` and have `C` replaced with the lexeme instantiating the POS [C]
    -- NB this fn clobbers the order of the child in the original syntax tree
    preTerm :: ConstituencyNodeLabel -> ConstituencyTree -> CompositionTree
    preTerm cnl@(CNodeLabel pre _) terminal = do
      terminalNode <- compose terminal
      case terminalNode of
        Node (EvaluatedSemNode (Syn.Const c _) _ _ _) Leaf Leaf -> do
          lex <- checkLexicon pre
          case lex of
            Just (expr, ty) -> let expr' = Syn.rename pre c expr in
              pure $ (semNode (expr') ty (Sem.runEval expr') cnl) Leaf terminalNode
            Nothing         -> pure $ Node (EmptySemNode cnl) Leaf terminalNode
        _ -> pure $ Node (EmptySemNode cnl) Leaf terminalNode

runComposition :: Frag.Fragment -> ConstituencyTree -> SemanticTree
runComposition frag synTree = runReader (compose synTree) frag
