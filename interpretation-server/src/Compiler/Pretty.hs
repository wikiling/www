{-# LANGUAGE NamedFieldPuns #-}

module Compiler.Pretty (
  ppexpr,
  pptype
) where

import qualified Compiler.Syntax as Syn
import qualified Compiler.Types as Ty
import qualified Compiler.TypeEnv as TE

import Prelude hiding ((<>))
import qualified Data.Map as Map
import Data.List (intersperse)
import Text.PrettyPrint

angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

parensIf ::  Bool -> Doc -> Doc
parensIf b = case b of
  True  -> parens
  False -> id

commaSep = punctuate $ char ','
commaSep1 = intersperse $ char ','

instance Pretty Syn.Lit where
  ppr _ l = case l of
    Syn.LInt i  -> text (show i)
    Syn.LBool b -> text (show b)

instance Pretty Syn.Binder where
  ppr _ (Syn.Binder n t) = text n <+> text "→"

instance Pretty Syn.Expr where
  ppr p e = case e of
    Syn.Var s -> text s
    Syn.Const c _ -> text c
    Syn.ELit l  -> ppr p l
    Syn.EBinder b -> (char 'λ') <> ppr 0 b
    Syn.App a b -> parensIf (p > 0) ((ppr (p + 1) a) <+> (ppr p b))
    Syn.Lam b e -> pClosure (char 'λ') b e
    Syn.Pred n t es -> text n <> ((parens . hsep . commaSep . (map $ ppr p)) es)
    Syn.EUnOp op e -> case op of
      Syn.Neg -> char '¬' <> (ppr p e)
      Syn.SetCompl -> (ppr p e) <> char '∁'
    Syn.EBinOp op e0 e1 -> case op of
      Syn.Conj -> infixSep '&' [e0,e1]
      Syn.Disj -> infixSep '|' [e0,e1]
      Syn.Impl -> infixSep '→' [e0,e1]
      Syn.Add -> infixSep '+' [e0,e1]
      Syn.Mul -> infixSep '*' [e0,e1]
      Syn.Sub -> infixSep '-' [e0,e1]
      Syn.Div -> infixSep '/' [e0,e1]
      Syn.SetUnion -> infixSep '∪' [e0,e1]
      Syn.SetInter -> infixSep '∩' [e0,e1]
      Syn.SetDiff -> infixSep '∖' [e0,e1]
      Syn.SetSubS -> infixSep '⊆' [e0,e1]
      Syn.SetMem -> infixSep '∈' [e0,e1]
    Syn.EQuant q b e -> case q of
      Syn.Univ -> pClosure (char '∀') b e
      Syn.Exis -> pClosure (char '∃') b e
    where
      pClosure sym b body = parensIf (p > 0) $
        sym <> ppr p b <+> ppr (p + 1) body
      infixSep c = hsep . (intersperse $ (char c)) . (map $ ppr p)

instance Pretty Syn.TyVar where
  ppr _ (Syn.TV t) = text t

instance Pretty Syn.Type where
  ppr _ (Syn.TyCon t) = text t
  ppr p (Syn.TyVar t) = ppr p t
  ppr p (Syn.TyFun a b) = angles ((ppr p a) <> text "," <> ppr p b)
    where
      isFunc Syn.TyFun{} = True
      isFunc _ = False

instance Pretty Syn.Decl where
  ppr p (Syn.Let n e) = (text n) <+> (text "=") <+> ppr p e
  ppr p (Syn.Typedef n t) = (text n) <> colon <+> ppr p t

instance Pretty Syn.TyScheme where
  ppr _ (Syn.Forall tvs ty) = let ppt = ppr 0 ty in
    if null tvs
      then ppt
      else (char '∀') <> (hcat $ commaSep1 $ map (ppr 0) tvs) <+> char '.' <+> ppt

instance Pretty TE.Env where
  ppr p TE.TypeEnv{TE.types} = let
    ts = Map.toList types
    pp1 (n,tyScheme) = text n <+> colon <+> ppr p tyScheme
    in vcat $ (map pp1 ts)

instance Show TE.Env where
  show = show . ppr 0

instance Show Syn.Expr where
  show = show . ppr 0

instance Show Syn.Binder where
  show = show . ppr 0

instance Show Syn.Type where
  show = show . ppr 0

instance Show Syn.TyScheme where
  show = show . ppr 0

instance Show Syn.Decl where
  show = show . ppr 0

instance Show Ty.TypeError where
  show (Ty.Mismatch a b) =
    "Expecting " ++ (pptype b) ++ " but got " ++ (pptype a)
  show (Ty.NotFunction e0 e1) =
    "Tried to apply non-function type: " ++ (ppexpr e0) ++ " to " ++ ppexpr e1
  show (Ty.NotInScope a) =
    "Variable " ++ a ++ " is not in scope"

ppexpr :: Syn.Expr -> String
ppexpr = render . ppr 0

pptype :: Syn.Type -> String
pptype = render . ppr 0