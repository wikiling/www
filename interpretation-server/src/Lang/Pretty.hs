module Lang.Pretty (
  ppexpr,
  pptype
) where

import qualified Lang.Syn as S
import qualified Lang.Type as T

import Text.PrettyPrint

class Pretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

instance Pretty S.Expr where
  ppr p ex = case ex of
    S.ETerm (S.TVar v)   -> text v
    S.ETerm (S.TConst c) -> text c
    S.ELit (S.LInt i)    -> text (show i)
    S.ELit (S.LBool b)   -> text (show b)
    S.App a b -> (parensIf (p>0) (ppr (p+1) a)) <+> (ppr p b)
    S.Lam x t a -> parensIf (p > 0) $
          char '\\'
      <+> parens (text x <+> char ':' <+> ppr p t)
      <+> text "->"
      <+> ppr (p+1) a

instance Pretty S.Type where
  ppr _ S.TyInt  = text "Int"
  ppr _ S.TyBool = text "Bool"
  ppr _ S.TyEnt  = text "Entity"
  ppr p (S.TyFunc a b) = (parensIf (isFunc a) (ppr p a)) <+> text "->" <+> ppr p b
    where
      isFunc S.TyFunc{} = True
      isFunc _ = False

instance Show S.Expr where
  show = show . pp

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