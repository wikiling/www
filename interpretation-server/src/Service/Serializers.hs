{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Serializers where

import Control.Applicative ((<|>))
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BS
import Data.Vector ((!), Vector)

import qualified Interpreter.Fragment as F
import qualified Interpreter.Composition as C
import Compiler.Pretty

parseTok :: JSON.Object -> Parser C.SynTree
parseTok obj = do
  tok <- obj .: "token"
  pure $ C.Node tok C.Leaf C.Leaf

parsePos :: JSON.Object -> Parser C.SynTree
parsePos obj = do
  pos <- obj .: "pos"
  cs <- obj .: "children"
  (c1, c2) <- parseChildren cs
  pure $ C.Node pos c1 c2

parseChildren :: JSON.Array -> Parser (C.SynTree, C.SynTree)
parseChildren cs = case length cs of
  0 -> pure $ (C.Leaf, C.Leaf)
  1 -> pure $ (parseChild 0, C.Leaf)
  2 -> pure $ (parseChild 0, parseChild 1)
  _ -> fail "must be binary tree"
  where
    parseChild i = case ((parseMaybe parseJSON $ cs!i)) of
      Nothing -> C.Leaf
      Just s -> s

instance JSON.FromJSON C.SynTree where
  parseJSON = JSON.withObject "SyntaxTree" $ \obj ->
    parseTok obj <|>
    parsePos obj

{-
instance JSON.ToJSON C.SemTree where
  toJSON Node 
  toJSON t = case t of
    C.Leaf -> null
    C.Node b c1 c2 -> show b (toJSON c1) (toJSON c2)
-}