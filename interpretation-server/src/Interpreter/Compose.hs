{-# LANGUAGE PatternSynonyms #-}

module Interpreter.Compose () where

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
type SemanticLabel = SLabel S.Expr (Either Inf.TypeError S.Type) Sem.Value ConstituencyLabel | SCLabel ConstituencyLabel
type SemanticTree = T.Tree SLabel

checkLexicon :: S.Name -> FragmentCtx (Maybe Frag.LexicalEntry)
checkLexicon name = asks (Map.lookup name)

pattern FnNode tDom tRan e <- T.Node (EExprLabel e (S.TyFun tDom tRan) _) _ _
pattern ArgNode t e <- T.Node (EExprLabel e t _) _ _

composeExprTree :: Frag.Fragment -> ConstituencyTree -> ExprTree
composeExprTree frag cTree = runReader (compose cTree) frag
  where
    compose :: ConstituencyTree -> FragmentCtx ExprTree
    compose (T.Node cnl@(CLabel label _) c0 c1) = case (c0,c1) of
      (T.Leaf, T.Leaf) -> do
        lex <- checkLexicon label
        case lex of
          Nothing -> pure $ T.Node (ECLabel cnl) T.Leaf T.Leaf
          Just (expr, ty) -> pure $ T.Node (EExprLabel expr ty cnl) T.Leaf T.Leaf
      (_, T.Leaf) -> composePreTerm cnl c0
      (T.Leaf, _) -> composePreTerm cnl c1
      _ -> do
        e0 <- compose c0
        e1 <- compose c1
        pure $ composeBinary cnl e0 e1

    composePreTerm cnl@(CLabel pre _) terminal = do
      terminalNode <- compose terminal
      case terminalNode of
        T.Node (ECLabel (CLabel label _)) _ _ -> do
          lex <- checkLexicon pre
          case lex of
            Just (expr, ty) -> pure $ T.Node (EExprLabel (S.rename pre label expr) ty cnl) T.Leaf terminalNode
            Nothing        -> pure $ T.Node (ECLabel cnl) T.Leaf terminalNode
        _ -> pure $ T.Node (ECLabel cnl) T.Leaf terminalNode

    composeBinary cnl c0 c1 = case c0 of
      FnNode t1Dom t1Ran e0 -> case c1 of
        ArgNode t2 e1 | Inf.unifiable t1Dom t2 -> appNode e0 e1
        _ -> noAppNode
      ArgNode t1 e0 -> case c1 of
        FnNode t2Dom t2Ran e1 | Inf.unifiable t2Dom t1 -> appNode e1 e0
        _ -> noAppNode
      _ -> noAppNode
      where
        appNode :: S.Expr -> S.Expr -> ExprTree
        appNode e0 e1 = let
          e = (S.App e0 e1)
          t = case Inf.inferExpr TyEnv.empty e of
            Right (S.Forall _ ty) -> ty
            Left e -> S.TyCon $ show e
          in
            T.Node (EExprLabel e t cnl) c0 c1

        noAppNode = T.Node (ECLabel cnl) c0 c1

{-
composeSemanticTree :: ExprTree -> SemanticTree
composeSemanticTree (T.Node eLabel c0 c1) = case eLabel of
  EExprLabel expr ty cl ->
  ECLabel cl -> 
-}