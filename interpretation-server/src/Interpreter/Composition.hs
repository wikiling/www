{-# LANGUAGE PatternSynonyms #-}

module Interpreter.Composition (
  Tree(..),
  SynTree,
  SemTree,
  SemNode(..),
  runComposition,
  printTree
) where

import qualified Data.Map as Map
import Data.Tree.Binary.Preorder (Tree(..), printTree)
import Control.Monad.Identity
import Control.Monad.Reader

import qualified Compiler.Syntax as Syn
import qualified Compiler.Parser as Parse
import Compiler.Pretty
import qualified Interpreter.Fragment as Frag
import qualified Interpreter.Evaluation as Sem

import Debug.Trace (trace, traceM)

data SemNode = SemNode Syn.Expr Syn.Type Sem.Value
type SemTree = Tree (Maybe SemNode)
type SynTree = Tree String

type FragmentCtx = Reader Frag.Fragment
type CompositionTree = FragmentCtx SemTree

instance Show SemNode where
  show (SemNode expr ty v) = show expr ++ " " ++ show ty ++ " " ++ show v

checkLexicon :: Syn.Name -> FragmentCtx (Maybe Frag.LexicalEntry)
checkLexicon name = asks (Map.lookup name)

apply :: Syn.Expr -> Syn.Expr -> Sem.Value
apply e1 e2 = Sem.runEval (Syn.App e1 e2)

semTree :: Syn.Expr -> Syn.Type -> Sem.Value -> (SemTree -> SemTree -> SemTree)
semTree e t v = Node (Just (SemNode e t v))

leafConst :: String -> SemTree
leafConst n = (semTree (Syn.ESym (Syn.SConst n)) Syn.TyEnt (Sem.VEnt n)) Leaf Leaf

saturatePredicativeExpr :: Syn.Expr -> String -> Syn.Expr
saturatePredicativeExpr expr p = expr

pattern FnNode tDom tRan e <- Node (Just (SemNode e (Syn.TyFunc tDom tRan) _)) _ _
pattern ArgNode t e <- Node (Just (SemNode e t _)) _ _

functionApp :: SemTree -> SemTree -> SemTree
functionApp b1 b2 = case b1 of
  FnNode t1Dom t1Ran e1 -> case b2 of
    ArgNode t2 e2 | t1Dom == t2 -> appNode e1 e2 t1Ran
    _ -> noApp "0"
  ArgNode t1 e1 -> case b2 of
    FnNode t2Dom t2Ran e2 | t2Dom == t1 -> appNode e2 e1 t2Ran
    _ -> noApp "1"
  _ -> noApp "2"
  where
    appNode :: Syn.Expr -> Syn.Expr -> Syn.Type -> SemTree
    appNode e1 e2 t = case apply e1 e2 of
      v@(Sem.VClosure s e c) -> (semTree e t v) b1 b2
      -- what's this case?
      v -> (semTree (Syn.App e1 e2) t v) b1 b2

    noApp n = (semTree (Syn.ESym (Syn.SConst n)) Syn.TyEnt (Sem.VEnt n)) b1 b2

compose :: SynTree -> CompositionTree
compose synTree = do
  case synTree of
    Node b Leaf Leaf -> do        -- leaf
      lex <- checkLexicon b
      case lex of
        Nothing -> pure $ leafConst b
        Just (expr, ty) -> pure $ (semTree expr ty (Sem.runEval expr)) Leaf Leaf
    Node b c Leaf -> preTerm b c  -- one child
    Node b Leaf c -> preTerm b c
    Node b c1 c2 -> do            -- two children
      s1 <- compose c1
      s2 <- compose c2
      pure $ functionApp s1 s2
  where
    -- (clobbers order of child `term`)
    preTerm :: String -> SynTree -> CompositionTree
    preTerm pre term = do
      sTerm <- compose term
      case sTerm of
        b@(Node (Just (SemNode (Syn.ESym (Syn.SConst c)) t v)) Leaf Leaf) -> do
          lex <- checkLexicon pre
          case lex of
            Just (expr, ty) -> let satExpr = saturatePredicativeExpr expr c in
              -- the `v` here should be replaced by an evaluation of `satExpr` once
              -- the semantics can handle the introduction of logical constants
              pure $ (semTree (satExpr) ty v) Leaf b
            Nothing         -> pure $ Node Nothing Leaf b
        c -> pure $ Node Nothing Leaf c

runComposition :: Frag.Fragment -> SynTree -> SemTree
runComposition frag synTree = runReader (compose synTree) frag
