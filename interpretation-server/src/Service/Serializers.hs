{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Serializers where

import Control.Applicative ((<|>))
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Data.Aeson
import qualified Data.HashMap.Strict as H
import GHC.Generics (Generic)

import qualified Interpret.Frag as F
import qualified Interpret.Comp as C

instance JSON.FromJSON C.SynTree where
  parseJSON = JSON.withObject "SyntaxTree" \obj ->
    Leaf <$> obj .: "token"
    <|>
    Branch <$> obj .: "pos" <*> obj .: "children"

instance JSON.ToJSON C.SemTree where
  toJSON t = case t of
    C.Branch b -> _
    C.Leaf   l -> _