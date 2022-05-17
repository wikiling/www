module Compile.Pretty (
  ppexpr,
  pptype
) where

import qualified Compile.Syn as S
import qualified Compile.Type as T

import Prelude hiding ((<>))
import Data.List (intersperse)
import Text.PrettyPrint

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

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

instance Pretty S.Lit where
  ppr _ l = case l of
    S.LInt i  -> text (show i)
    S.LBool b -> text (show b)

instance Pretty S.Sym where
  ppr _ t = case t of
    S.SVar v   -> text v
    S.SConst c -> text c

instance Pretty S.Expr where
  ppr p e = case e of
    S.ESym t -> ppr p t
    S.ELit l  -> ppr p l
    S.App a b -> (parensIf (p>0) (ppr (p+1) a)) <+> (ppr p b)
    S.Lam n t body -> parensIf (p > 0) $
      char 'λ' <> text n <> char ':' <+> ppr p t
      <+> text "->"
      <+> ppr (p+1) body
    S.Pred n ts -> text n <> ((parens . hsep . commaSep . (map $ ppr p)) ts)
    S.Neg e -> char '¬' <> (ppr p e)  
    S.Conj e1 e2 -> conjSep p [e1,e2]
    S.Disj e1 e2 -> disjSep p [e1,e2]
    S.Impl e1 e2 -> implSep p [e1,e2]
    -- S.UnivQ n t body ->
    -- S.ExisQ n t body ->
    S.Add e1 e2 -> addSep p [e1,e2]
    S.Mul e1 e2 -> mulSep p [e1,e2]
    S.Sub e1 e2 -> subSep p [e1,e2]
    S.Div e1 e2 -> divSep p [e1,e2]

instance Pretty S.Type where
  ppr _ S.TyInt  = text "n"
  ppr _ S.TyBool = text "t"
  ppr _ S.TyEnt  = text "e"
  ppr p (S.TyFunc a b) = (parensIf (isFunc a) (ppr p a)) <+> text "->" <+> ppr p b
    where
      isFunc S.TyFunc{} = True
      isFunc _ = False

instance Show S.Expr where
  show = show . ppr 0

instance Show T.TypeError where
  show (T.Mismatch a b) =
    "Expecting " ++ (pptype b) ++ " but got " ++ (pptype a)
  show (T.NotFunction a) =
    "Tried to apply to non-function type: " ++ (pptype a)
  show (T.NotInScope a) =
    "Variable " ++ a ++ " is not in scope"

ppexpr :: S.Expr -> String
ppexpr = render . ppr 0

pptype :: S.Type -> String
pptype = render . ppr 0