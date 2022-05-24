module Interpreter.Composition (
  Tree(..),
  SynTree,
  SemTree,
  SemNode(..),
  runComposition
) where

import qualified Data.Map as Map
import Data.Tree.Binary.Preorder (Tree(..))
import Control.Monad.Identity
import Control.Monad.Reader

import qualified Compiler.Syntax as Syn
import qualified Compiler.Parser as Parse
import qualified Interpreter.Fragment as Frag
import qualified Interpreter.Semantics as Sem

data SemNode = SemNode Syn.Expr Syn.Type Sem.Value
type SemTree = Tree (Maybe SemNode)
type SynTree = Tree String

type FragmentCtx = Reader Frag.Fragment
type CompositionTree = FragmentCtx SemTree

instance Show SemNode where
  show (SemNode expr ty v) = show expr ++ show ty ++ show v

checkLexicon :: Syn.Name -> FragmentCtx (Maybe Frag.LexicalEntry)
checkLexicon name = asks (Map.lookup name)

apply :: Sem.Value -> Sem.Value -> Sem.Value
apply v1 v2 = case Sem.apply v1 v2 of
  Identity v -> v

semTree :: Syn.Expr -> Syn.Type -> Sem.Value -> (SemTree -> SemTree -> SemTree)
semTree e t v = Node (Just (SemNode e t v))

leafConst :: String -> SemTree
leafConst n = (semTree (Syn.ESym (Syn.SConst n)) Syn.TyEnt (Sem.VEnt n)) Leaf Leaf

saturatePredicativeExpr :: Syn.Expr -> String -> Syn.Expr
saturatePredicativeExpr expr p = expr

functionApp :: SemTree -> SemTree -> SemTree
functionApp b1 b2 = case b1 of
  Node (Just (SemNode _ (Syn.TyFunc t1Dom t1Ran) v1@(Sem.VClosure _ _ _))) _ _ ->
    case b2 of
      Node Nothing _ _ -> Node Nothing b1 b2
      Node (Just (SemNode _ t2 v2)) _ _ -> if t1Dom == t2
        then let v = apply v1 v2 in (semTree (closExp v) t1Ran v) b1 b2
        else Node Nothing b1 b2
  Node (Just _) _ _ -> functionApp b2 b1
  Node Nothing _ _ -> Node Nothing b1 b2
  where
    closExp :: Sem.Value -> Syn.Expr
    closExp (Sem.VClosure s e c) = e

compose :: SynTree -> CompositionTree
compose synTree = case synTree of
  Node b Leaf Leaf -> do -- leaf
    lex <- checkLexicon b
    case lex of
      Nothing -> pure $ leafConst b
      Just (expr, ty) -> pure $ (semTree expr ty (Sem.runEval expr)) Leaf Leaf
  Node b c Leaf -> preTerm b c -- one child (clobbers order of child)
  Node b Leaf c -> preTerm b c
  Node b c1 c2 -> do -- two children
    s1 <- compose c1
    s2 <- compose c2
    pure $ functionApp s1 s2
  where
    preTerm :: String -> SynTree -> CompositionTree
    preTerm pre term = do
      sTerm <- compose term
      case sTerm of
        b@(Node (Just (SemNode (Syn.ESym (Syn.SConst c)) t v)) Leaf Leaf) -> do
          lex <- checkLexicon pre
          case lex of
            Just (expr, ty) -> pure $ (semTree (saturatePredicativeExpr expr c) t v) Leaf b
            _ -> pure $ Node Nothing Leaf b
        c -> pure $ Node Nothing Leaf c

runComposition :: Frag.Fragment -> SynTree -> SemTree
runComposition frag synTree = runReader (compose synTree) frag
