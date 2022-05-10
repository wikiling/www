{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Models0
  ( SyntaxTree (..),
    SemTree (..)
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

data Term = Const String | Var Int deriving (Eq, Ord, Generic, JSON.ToJSON, JSON.FromJSON)

data Abstract = MkAbstract Int LF deriving (Eq, Ord, Generic, JSON.ToJSON, JSON.FromJSON)

data LF = Rel String [Term] 
        | Eq   Term Term
        | Neg  LF 
        | Impl LF LF 
        | Equi LF LF 
        | Conj [LF]
        | Disj [LF]
        | Abs Abstract
     deriving (Eq, Ord, Generic, JSON.ToJSON, JSON.FromJSON)

instance Show Term where
  show (Const name) = name 
  show (Var i)      = 'x': show i

instance Show Abstract where 
  show (MkAbstract i lf) = 
   "(λx" ++ show i ++ " " ++ show lf ++ ")"

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

data Category = Cat String (String -> Abstract)
instance Show Category where
  show (Cat s _) = show s
instance JSON.ToJSON Category where
  toJSON (Cat s _) = JSON.toJSON s

lookupAbstract :: String -> (String -> Abstract)
-- [[V]] = λy.λx.λe.V(e)(y)(x), type: <R,<e,<e,<v,t>>>>
lookupAbstract "V" rel = MkAbstract 1 (Abs $ MkAbstract 2 (Abs $ MkAbstract 3 (Rel rel [Var 1, Var 2, Var 3])))

type Pos = String

data SyntaxTree = Node Pos Category [SyntaxTree] | Leaf Pos Term
  deriving (Show, Generic, JSON.ToJSON)

syntaxNodeParser :: JSON.Object -> Parser SyntaxTree
syntaxNodeParser obj =
  Leaf <$> obj .: "id" <*> (Const <$> (obj .: "token"))
    <|>
  Node <$> obj .: "id" <*> (Cat <$> (obj .: "pos") <*> (lookupAbstract <$> (obj .: "pos"))) <*> obj .: "children"

instance JSON.FromJSON SyntaxTree where
  parseJSON = JSON.withObject "SyntaxTree" syntaxNodeParser

data SemTree = SNode Pos LF [SemTree] | SLeaf Pos Term
  deriving (Show, Generic, JSON.ToJSON)

-- interpretTree :: SyntaxTree -> SemTree
-- interpretTree (Leaf _ (Const c)) = SLeaf pos c
-- interpretTree (Node pos cat [c1, c2]) = SNode pos [fmap interpretTree [c1, c2]]
-- interpretTree (Node pos (Cat cat lam) [(Leaf _ (Const c))]) = SNode pos $ Abs $ lam c
-- interpretTree (Leaf pos (Const const)) = SLeaf pos (Const const)
