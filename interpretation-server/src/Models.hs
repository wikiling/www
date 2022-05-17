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

import qualified Compile.Frag as F
import qualified Compile.Comp as C

type Const = String
data Pos = String deriving (Show, Generic, JSON.FromJSON)
type SyntaxTree = C.BTree Const Pos

syntaxTreeParser :: JSON.Object -> Parser SyntaxTree
syntaxTreeParser obj =
  Leaf <$> obj .: "token"
  <|>
  Branch <$> obj .: "pos" <*> obj .: "children"

instance JSON.FromJSON SyntaxTree where
  parseJSON = JSON.withObject "SyntaxTree" syntaxTreeParser