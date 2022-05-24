module Compiler.Pretty (
  ppexpr,
  pptype
) where

import qualified Compiler.Syntax as Syn
import qualified Compiler.Types as Ty

import Prelude hiding ((<>))
import Data.List (intersperse)
import Text.PrettyPrint

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
conjSep p = binarySep '∧' p
disjSep p = binarySep '∨' p
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
    Syn.ESym t -> ppr p t
    Syn.ELit l  -> ppr p l
    Syn.App a b -> (parensIf (p>0) (ppr (p+1) a)) <+> (ppr p b)
    Syn.Lam n t body -> parensIf (p > 0) $
      char 'λ' <> text n <> char ':' <> ppr p t
      <+> text "->"
      <+> ppr (p+1) body
    Syn.Pred n ts -> text n <> ((parens . hsep . commaSep . (map $ ppr p)) ts)
    Syn.Neg e -> char '¬' <> (ppr p e)  
    Syn.Conj e1 e2 -> conjSep p [e1,e2]
    Syn.Disj e1 e2 -> disjSep p [e1,e2]
    Syn.Impl e1 e2 -> implSep p [e1,e2]
    -- Syn.UnivQ n t body ->
    -- Syn.ExisQ n t body ->
    Syn.Add e1 e2 -> addSep p [e1,e2]
    Syn.Mul e1 e2 -> mulSep p [e1,e2]
    Syn.Sub e1 e2 -> subSep p [e1,e2]
    Syn.Div e1 e2 -> divSep p [e1,e2]

instance Pretty Syn.Type where
  ppr _ Syn.TyInt  = text "n"
  ppr _ Syn.TyBool = text "t"
  ppr _ Syn.TyEnt  = text "e"
  ppr _ Syn.TyEvent = text "v"
  ppr p (Syn.TyFunc a b) = (parensIf (isFunc a) (ppr p a)) <+> text "->" <+> ppr p b
    where
      isFunc Syn.TyFunc{} = True
      isFunc _ = False

instance Show Syn.Expr where
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