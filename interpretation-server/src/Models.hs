{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Models where

import Control.Applicative ((<|>))
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Data.Aeson
import qualified Data.HashMap.Strict as H
import GHC.Generics (Generic)

import FOL (Term, Func)

type Const = String
data Pos = String deriving (Show, Generic, JSON.FromJSON)
data SyntaxTree = Leaf Const | Branch Pos [SyntaxTree, SyntaxTree]


toVP :: SyntaxTree -> Term -> Term
toVP (Branch "VP" [np@(Branch "NP" _), v'@(Branch "V'" _)] = toNP np $ toV' v' 

toNP :: SyntaxTree -> Term
toNP (Branch "NP" [Leaf token]) -> token

toV' :: SyntaxTree -> Term -> Term
toV' (Branch "V'" [v@(Branch "V" _), np@(Branch "NP" _)] = toNP np $ toV v

toV :: SyntaxTree -> Term -> Term -> Term
toV (Branch "V" [Leaf token]) -> Func 

syntaxTreeParser :: JSON.Object -> Parser SyntaxTree
syntaxTreeParser obj =
  Leaf <$> obj .: "token"
  <|>
  Branch <$> obj .: "pos" <*> obj .: "children"

instance JSON.FromJSON SyntaxTree where
  parseJSON = JSON.withObject "SyntaxTree" syntaxTreeParser