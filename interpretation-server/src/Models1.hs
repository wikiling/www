{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Models1
  ( SyntaxTree (..),
    LF,
    transVP
  )
where

import Control.Applicative ((<|>))
import Data.Aeson ((.:))
import Data.Text (Text)
import Data.List
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Parser as P
import Data.Aeson.Types (Parser)
import GHC.Generics (Generic)

data Type = E | V | T | F (Type, Type)

instance Show Type where
  show (F (t, t)) = "<" ++ t ++ ", " ++ t ++ ">"
  show E      = "e"
  show V      = "v"
  show T      = "t"

type Pos = String
data Tree a b = Ep | Leaf Pos a | Branch Pos b [Tree a b] 
                   deriving Eq

instance (Show a, Show b) => Show (Tree a b) where
  show Ep              = "[]"
  show (Leaf _ t)      = show t
  show (Branch _ l ts) = "[." ++ show l  ++ " " 
                            ++ show ts ++ "]"

type CatLabel = String
type Phon     = String
data Cat      = Cat Phon CatLabel deriving Eq

instance Show Cat where
  show (Cat phon "lexeme") = phon
  show (Cat _ label) = label

phon :: Cat -> String
phon (Cat ph _) = ph

catLabel :: Cat -> CatLabel
catLabel (Cat _ label) = label

type SyntaxTree = Tree Cat Cat

data Term = Const String | Var Type Int deriving (Eq,Ord)

data Abstract = Lam Var LF deriving (Eq,Ord) 

data LF = Rel String [Term] 
        | Eq   Term Term
        | Neg  LF 
        | Impl LF LF 
        | Equi LF LF 
        | Conj [LF]
        | Disj [LF]
        | Abs Abstract
     deriving (Eq,Ord)

instance Show Term where
  show (Const name) = name 
  show (Var i t)      = "x" ++ show i ++ ":" ++ show t

instance Show Abstract where 
  show (MkAbstract i lf) = 
   "(λ.x" ++ show i ++ " " ++ show lf ++ ")"

instance Show LF where
  show (Rel r args)   = r ++ show args
  show (Eq t1 t2)     = show t1 ++ "==" ++ show t2
  show (Neg lf)       = '~': (show lf)
  show (Impl lf1 lf2) = "(" ++ show lf1 ++ "==>" 
                            ++ show lf2 ++ ")"
  show (Equi lf1 lf2) = "(" ++ show lf1 ++ "<=>" 
                            ++ show lf2 ++ ")"
  show (Conj [])      = "true" 
  show (Conj lfs)     = "conj" ++ concat [ show lfs ]
  show (Disj [])      = "false" 
  show (Disj lfs)     = "disj" ++ concat [ show lfs ]

transNP :: SyntaxTree -> Term
transNP (Leaf _ (Cat phon "lexeme")) = Const phon

transV :: SyntaxTree -> (Term -> (Term -> (Term -> LF)))
transV (Branch _ (Cat _ "V") 
                [Leaf _ (Cat phon "lexeme")]) = 
        \dobj ->
          \subj ->
            \event -> Rel phon [event, dobj, subj]

transVBar :: SyntaxTree -> (Term -> (Term -> LF))
transVBar (Branch _ (Cat _ "V'") 
                [v@(Branch _ (Cat _ "V") _), np@(Branch _ (Cat _ "NP") _)]) = 
        \subj -> (transV v) (transNP np) subj

-- [[V]] = λy.λx.λe.V(e)(y)(x), type: <<e,<e,<v,t>>>>
transVP :: SyntaxTree -> (Term -> LF)
transVP (Branch _ (Cat _ "VP") 
                [np@(Branch _ (Cat _ "NP") _), vbar@(Branch _ (Cat _ "VBar") _)]) = 
        \event -> (transVBar vbar) (transNP np) event

syntaxTreeParser :: JSON.Object -> Parser SyntaxTree
syntaxTreeParser obj =
  Leaf <$> obj .: "id" <*> (Cat <$> obj .: "token" <*> obj .: "pos")
    <|>
  Branch <$> obj .: "id" <*> (Cat <$> obj .: "token" <*> obj .: "pos") <*> obj .: "children"

instance JSON.FromJSON SyntaxTree where
  parseJSON = JSON.withObject "SyntaxTree" syntaxTreeParser
