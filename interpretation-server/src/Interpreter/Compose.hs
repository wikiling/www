{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}

module Interpreter.Compose (
  T.Tree(..),
  ConstituencyTree,
  ConstituencyLabel(..),
  ExprTree,
  ExprLabel(..),
  SemanticTree,
  SemanticLabel(..),
  compose,
  T.printTree
  ) where

import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Tree.Binary.Preorder as T
import Control.Monad.Identity
import Control.Monad.Reader

import Compiler.Pretty
import qualified Compiler.Syntax as S
import qualified Compiler.Inference as Inf
import qualified Compiler.TypeEnv as TyEnv
import qualified Interpreter.Fragment as Frag
import qualified Interpreter.Evaluation as Sem

-- | Tree position
type Pos = String
-- | Label + position
data ConstituencyLabel = CLabel String Pos deriving (Show, Generic)
type ConstituencyTree = T.Tree ConstituencyLabel

-- | Intermediate composition (an AST)
type FragmentCtx = Reader Frag.Fragment
data ExprLabel = EExprLabel S.Expr S.Type ConstituencyLabel | ECLabel ConstituencyLabel
type ExprTree = T.Tree ExprLabel

-- | Evaluated composition
data SemanticLabel = SLabel S.Expr S.Type Sem.Value ConstituencyLabel | SCLabel ConstituencyLabel
type SemanticTree = T.Tree SemanticLabel

instance Show SemanticLabel where
  show (SLabel expr ty v _) = show expr ++ " " ++ show ty ++ " " ++ show v
  show (SCLabel _) = "<empty>"

checkLexicon :: S.Name -> FragmentCtx (Maybe Frag.LexicalEntry)
checkLexicon name = asks (Map.lookup name)

pattern FnNode tDom tRan e <- T.Node (EExprLabel e (S.TyFun tDom tRan) _) _ _
pattern ArgNode t e <- T.Node (EExprLabel e t _) _ _
pattern BindNode b <- T.Node (EExprLabel (S.EBinder b) _ _) _ _

-- | Compose an AST from a constituency tree, pulling lexical
--   entries from the grammar and combining nodes via our composition operations.
composeExprTree :: Frag.Fragment -> ConstituencyTree -> ExprTree
composeExprTree frag cTree = runReader (compose cTree) frag
  where
    compose :: ConstituencyTree -> FragmentCtx ExprTree
    compose (T.Node cl@(CLabel label _) c0 c1) = case (c0,c1) of
      (T.Leaf, T.Leaf) -> do
        lex <- checkLexicon label
        case lex of
          Nothing -> pure $ T.Node (ECLabel cl) T.Leaf T.Leaf
          Just (expr, ty) -> pure $ T.Node (EExprLabel expr ty cl) T.Leaf T.Leaf
      (_, T.Leaf) -> composePreTerm cl c0
      (T.Leaf, _) -> composePreTerm cl c1
      _ -> do
        e0 <- compose c0
        e1 <- compose c1
        pure $ composeBinary cl e0 e1

    composePreTerm :: ConstituencyLabel -> ConstituencyTree -> FragmentCtx ExprTree
    composePreTerm cl@(CLabel pre _) terminal = do
      terminalNode <- compose terminal
      case terminalNode of
        T.Node (ECLabel (CLabel label _)) _ _ -> do
          lex <- checkLexicon pre
          case lex of
            Just (expr, ty) -> pure $ T.Node (EExprLabel (S.rename pre label expr) ty cl) T.Leaf terminalNode
            Nothing        -> pure $ T.Node (ECLabel cl) T.Leaf terminalNode
        _ -> pure $ T.Node (ECLabel cl) T.Leaf terminalNode

    composeBinary :: ConstituencyLabel -> ExprTree -> ExprTree -> ExprTree
    composeBinary cl c0 c1 = case c0 of
      BindNode b -> bindNode b c1
      FnNode t1Dom t1Ran e0 -> case c1 of
        BindNode b -> bindNode b c0
        ArgNode t2 e1 | Inf.unifiable t1Dom t2 -> appNode e0 e1
        _ -> fallthru
      ArgNode t1 e0 -> case c1 of
        BindNode b -> bindNode b c0
        FnNode t2Dom t2Ran e1 | Inf.unifiable t2Dom t1 -> appNode e1 e0
        _ -> fallthru
      _ -> fallthru
      where
        bindNode :: S.Binder -> ExprTree -> ExprTree
        bindNode b@(S.Binder _ t0) (T.Node l _ _) = case l of
          EExprLabel e t1 _ -> T.Node (EExprLabel (S.Lam b e) (S.TyFun t0 t1) cl) c0 c1
          _ -> fallthru

        appNode :: S.Expr -> S.Expr -> ExprTree
        appNode e0 e1 = let
          e = (S.App e0 e1)
          t = case Inf.inferExpr TyEnv.empty e of
            Right (S.Forall _ ty) -> ty
            Left e -> S.TyCon $ show e -- fixme
          in
            T.Node (EExprLabel e t cl) c0 c1

        fallthru = T.Node (ECLabel cl) c0 c1

mapAccumTree :: (c -> a -> (c, b)) -> c -> T.Tree a -> (c, T.Tree b)
mapAccumTree f s t = go s t
  where
    go s T.Leaf = (s, T.Leaf)
    go s (T.Node x l r) = let
      (s', x') = f s x
      (sl, l') = go s l
      (sr, r') = go sl r
      in (s', (T.Node x' l' r'))

-- | Type an expression tree. The tree's expression nodes already
--   have types, but here we resolve any type variables to their most
--   general type.
typeExprTree :: ExprTree -> ExprTree
typeExprTree e = snd $ mapAccumTree infer TyEnv.empty e
  where
    infer :: TyEnv.Env -> ExprLabel -> (TyEnv.Env, ExprLabel)
    infer env l = case l of
      ECLabel{} -> (env, l)
      (EExprLabel e _ cl) -> case Inf.inferExpr env e of
        Left err -> (env, l) -- fixme! don't swallow errors
        Right gTy@(S.Forall as ty) -> (env', EExprLabel e ty cl) where
          env' = case e of
            (S.EBinder (S.Binder n t)) -> TyEnv.extend env (n, gTy)
            _ -> env

-- | Evaluate an expression tree.
composeSemanticTree :: ExprTree -> SemanticTree
composeSemanticTree e = fmap compose e
  where
    compose :: ExprLabel -> SemanticLabel
    compose eLabel = case eLabel of
      EExprLabel expr ty cl -> SLabel expr ty (Sem.runEval expr) cl
      ECLabel cl -> SCLabel cl


compose f c = (composeSemanticTree . typeExprTree) (composeExprTree f c)