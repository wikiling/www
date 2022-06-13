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

-- | Initial composition: instantiate lexical entries
type ExprTree = T.Tree (Maybe S.Expr, ConstituencyLabel)

-- | Intermediate composition: assign types
data TypedExprLabel = TELabel S.Expr S.Type
type TypedExprTree = T.Tree (Maybe TypedExprLabel, ConstituencyLabel)

-- | Intermediate composition: apply composition operations

-- | Evaluated composition
data SemanticLabel = SLabel S.Expr S.Type Sem.Value
type SemanticTree = T.Tree (Maybe SemanticLabel, ConstituencyLabel)

type FragmentCtx = Reader Frag.Fragment

instance Show SemanticLabel where
  show (SLabel expr ty v _) = show expr ++ " " ++ show ty ++ " " ++ show v
  show (SCLabel _) = "<empty>"

checkLexicon :: S.Name -> FragmentCtx (Maybe Frag.LexicalEntry)
checkLexicon name = asks (Map.lookup name)

mapAccumTree :: (c -> a -> (c, b)) -> c -> T.Tree a -> (c, T.Tree b)
mapAccumTree f s t = go s t
  where
    go s T.Leaf = (s, T.Leaf)
    go s (T.Node x l r) = let
      (s', x') = f s x
      (sl, l') = go s l
      (sr, r') = go sl r
      in (s', (T.Node x' l' r'))

-- | Construct an expression tree from a constituency tree, pulling lexical
--   entries from the fragment.
mkExprTree :: Frag.Fragment -> ConstituencyTree -> ExprTree
mkExprTree frag cTree = runReader (mk cTree) frag
  where
    mk :: ConstituencyTree -> FragmentCtx ExprTree
    mk (T.Node cl@(CLabel cLabel _) c0 c1) = case (c0,c1) of
      (T.Leaf, T.Leaf) -> do
        eLabel <- checkLexicon cLabel
        pure $ T.Node (eLabel, cl) T.Leaf T.Leaf
      (_, T.Leaf) -> preTerm cl c0
      (T.Leaf, _) -> preTerm cl c1
      _ -> T.Node (Nothing, cl) c0 c1

    preTerm :: ConstituencyLabel -> ConstituencyTree -> FragmentCtx ExprTree
    preTerm cl@(CLabel cLabelPre _) terminal = do
      terminalNode <- compose terminal
      lex <- checkLexicon cLabelPre

      let eLabel = case lex of
        Just expr -> case terminalNode of
          T.Node (ECLabel (CLabel cLabelTerm _)) _ _ -> Just S.rename cLabelPre cLabelTerm expr
          _ -> Nothing
        Nothing -> Nothing

      pure $ T.Node (eLabel, cl) T.Leaf terminalNode

-- | Type an expression tree
typeExprTree :: ExprTree -> TypedExprTree
typeExprTree e = snd $ mapAccumTree ty TyEnv.empty e
  where
    ty :: TyEnv.Env -> ExprLabel -> (TyEnv.Env, ExprLabel)
    ty env l = case l of
      ECLabel{} -> (env, l)
      (EExprLabel e _ cl) -> case Inf.inferExpr env e of
        Left err -> (env, l) -- fixme! don't swallow errors
        Right gTy@(S.Forall as ty) -> (env', EExprLabel e ty cl) where
          env' = case e of
            (S.EBinder (S.Binder n t)) -> TyEnv.extend env (n, gTy)
            _ -> env

pattern FnNode tDom tRan e <- T.Node (Just e _, _) (S.TyFun tDom tRan), _) _ _
pattern ArgNode t e <- T.Node (Just e t, _) _ _
pattern BindNode b <- T.Node (Just (S.EBinder b)) _, _) _ _

-- | Build expressions upward via our composition operations.
composeExprTree :: TypedExprTree -> TypedExprTree
composeExprTree (T.Node (_, cl) c0 c1) = case c0 of
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
    bindNode :: S.Binder -> TypedExprTree -> TypedExprTree
    bindNode b@(S.Binder _ t0) (T.Node l _ _) = case l of
      EExprLabel e t1 _ -> saturate (S.Lam b e) (S.TyFun t0 t1)
      _ -> fallthru

    appNode :: S.Expr -> S.Expr -> TypedExprTree
    appNode e0 e1 = saturate e t
      where
        e = (S.App e0 e1)
        t = case Inf.inferExpr TyEnv.empty e of
          Right (S.Forall _ ty) -> ty
          Left e -> S.TyCon $ show e -- fixme

    saturate e t = T.Node (Just e t, cl) c0 c1
    fallthru = T.Node (Nothing, cl) c0 c1

-- | Evaluate an expression tree.
evalExprTree :: TypedExprTree -> SemanticTree
evalExprTree e = fmap eval e
  where
    eval :: ExprLabel -> SemanticLabel
    eval eLabel = case eLabel of
      EExprLabel expr ty cl -> SLabel expr ty (Sem.runEval expr) cl
      ECLabel cl -> SCLabel cl

compose f c = (evalExprTree . composeExprTree . typeExprTree) (mkExprTree f c)