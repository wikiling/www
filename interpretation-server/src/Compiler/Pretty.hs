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

binarySep c p = hsep . (intersperse $ (char c)) . (map $ ppr p)
conjSep p = binarySep '&' p
disjSep p = binarySep '|' p
implSep p = binarySep '→' p
addSep p = binarySep '+' p
subSep p = binarySep '-' p
mulSep p = binarySep '*' p
divSep p = binarySep '/' p

instance Pretty Syn.Lit where
  ppr _ l = case l of
    Syn.LInt i  -> text (show i)
    Syn.LBool b -> text (show b)

instance Pretty Syn.Sym where
  ppr _ t = case t of
    Syn.SVar v   -> text v
    Syn.SConst c -> text c

instance Pretty Syn.Expr where
  ppr p e = case e of
    Syn.ESym s _ -> ppr p s
    Syn.ELit l  -> ppr p l
    Syn.App a b -> parensIf (p > 0) ((ppr (p + 1) a) <+> (ppr p b))
    Syn.Lam n t body -> pLam (char 'λ') n t body
    Syn.Pred n ts -> text n <> ((parens . hsep . commaSep . (map $ ppr p)) ts)
    Syn.EUnOp op -> case op of
      Syn.Neg e -> char '¬' <> (ppr p e)  
    Syn.EBinOp op -> case op of
      Syn.Conj e1 e2 -> conjSep p [e1,e2]
      Syn.Disj e1 e2 -> disjSep p [e1,e2]
      Syn.Impl e1 e2 -> implSep p [e1,e2]
      Syn.Add e1 e2 -> addSep p [e1,e2]
      Syn.Mul e1 e2 -> mulSep p [e1,e2]
      Syn.Sub e1 e2 -> subSep p [e1,e2]
      Syn.Div e1 e2 -> divSep p [e1,e2]
    Syn.UnivQ e0 e1 -> pQuant (char '∀') e0 e1
    Syn.ExisQ e0 e1 -> pQuant (char '∃') e0 e1
    where
      pQuant sym e0 e1 = parensIf (p > 0) $
        sym
        <> ppr p e0
        <+> text "→"
        <+> ppr (p + 1) e1
      pLam sym n t body = parensIf (p > 0) $
        sym <> text n <> char ':' <> ppr p t
        <+> text "→"
        <+> ppr (p + 1) body

instance Pretty Syn.Type where
  ppr _ (Syn.TyCon t) = text t
  ppr _ (Syn.TyVar (Syn.TV t)) = text t
  ppr p (Syn.TyFunc a b) = angles ((ppr p a) <> text "," <> ppr p b)
    where
      isFunc Syn.TyFunc{} = True
      isFunc _ = False

instance Show Syn.Expr where
  show = show . ppr 0

instance Show Syn.Type where
  show = show . ppr 0

instance Show Ty.TypeError where
  show (Ty.Mismatch a b) =
    "Expecting " ++ (pptype b) ++ " but got " ++ (pptype a)
  show (Ty.NotFunction a) =
    "Tried to apply to non-function type: " ++ (pptype a)
  show (Ty.NotInScope a) =
    "Variable " ++ a ++ " is not in scope"

ppexpr :: Syn.Expr -> String
ppexpr = render . ppr 0

pptype :: Syn.Type -> String
pptype = render . ppr 0