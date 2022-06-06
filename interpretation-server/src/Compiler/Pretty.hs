module Compiler.Pretty (
  ppexpr,
  pptype
) where

import qualified Compiler.Syntax as Syn
import qualified Compiler.Types as Ty

import Prelude hiding ((<>))
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
wrap d1 d2 = d1 <> d2 <> d1 

instance Pretty Syn.Lit where
  ppr _ l = case l of
    Syn.LInt i  -> text (show i)
    Syn.LBool b -> text (show b)

instance Pretty Syn.Expr where
  ppr p e = case e of
    Syn.EVar s -> text s
    Syn.EConst c t -> text c <> text ":" <> ppr p t
    Syn.ELit l  -> ppr p l
    Syn.App a b -> parensIf (p > 0) ((ppr (p + 1) a) <+> (ppr p b))
    Syn.Lam n t e -> pBinder (char 'λ') n t e
    Syn.Pred n ts -> text n <> ((parens . hsep . commaSep . (map $ ppr p)) ts)
    Syn.EUnOp op -> case op of
      Syn.Neg e -> char '¬' <> (ppr p e)
      Syn.SetCompl e -> (ppr p e) <> char '∁'
    Syn.EBinOp op -> case op of
      Syn.Conj e0 e1 -> infixSep '&' [e0,e1]
      Syn.Disj e0 e1 -> infixSep '|' [e0,e1]
      Syn.Impl e0 e1 -> infixSep '→' [e0,e1]
      Syn.Add e0 e1 -> infixSep '+' [e0,e1]
      Syn.Mul e0 e1 -> infixSep '*' [e0,e1]
      Syn.Sub e0 e1 -> infixSep '-' [e0,e1]
      Syn.Div e0 e1 -> infixSep '/' [e0,e1]
      Syn.SetUnion e0 e1 -> infixSep '∪' [e0,e1]
      Syn.SetInter e0 e1 -> infixSep '∩' [e0,e1]
      Syn.SetDiff e0 e1 -> infixSep '∖' [e0,e1]
      Syn.SetSubS e0 e1 -> infixSep '⊆' [e0,e1]
      Syn.SetMem e0 e1 -> infixSep '∈' [e0,e1]
    Syn.UnivQ n t e -> pBinder (char '∀') n t e
    Syn.ExisQ n t e -> pBinder (char '∃') n t e
    where
      pBinder sym n t body = parensIf (p > 0) $
        sym <> text n <> char ':' <> ppr p t
        <+> text "→"
        <+> ppr (p + 1) body
      infixSep c = hsep . (intersperse $ (char c)) . (map $ ppr p)

instance Pretty Syn.Type where
  ppr _ (Syn.TyCon t) = text t
  ppr _ (Syn.TyVar (Syn.TV t)) = text t
  ppr p (Syn.TyFunc a b) = angles ((ppr p a) <> text "," <> ppr p b)
    where
      isFunc Syn.TyFunc{} = True
      isFunc _ = False

instance Pretty Syn.Decl where
  ppr p (Syn.Let n e) = (text n) <+> (text "=") <+> ppr p e
  ppr p (Syn.Typedef n t) = (text n) <> text ":" <+> ppr p t

instance Show Syn.Expr where
  show = show . ppr 0

instance Show Syn.Type where
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