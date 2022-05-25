{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Service.Serializers where

import Control.Applicative ((<|>))
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Vector ((!))

import qualified Interpreter.Fragment as F
import qualified Interpreter.Composition as C
import Compiler.Pretty

parseSynNode :: Object -> Parser C.SynTree
parseSynNode obj = do
  label <- obj .: "label"
  cs <- obj .:? "children"
  (c1, c2) <- parseSynChildren cs
  pure $ C.Node label c1 c2

parseSynChildren :: Maybe Array -> Parser (C.SynTree, C.SynTree)
parseSynChildren Nothing = pure $ (C.Leaf, C.Leaf)
parseSynChildren (Just cs) = case length cs of
  0 -> pure $ (C.Leaf, C.Leaf)
  1 -> pure $ (parseChild 0, C.Leaf)
  2 -> pure $ (parseChild 0, parseChild 1)
  _ -> fail "must be binary tree"
  where
    parseChild :: Int -> C.SynTree
    parseChild i = case ((parseMaybe parseJSON $ cs!i) :: Maybe C.SynTree) of
      Nothing -> C.Leaf
      Just s  -> s

instance FromJSON C.SynTree where
  parseJSON = withObject "SyntaxTree" parseSynNode

instance ToJSON C.SemTree where
  toJSON (C.Node s c1 c2) = object $ (serializeSemNode s) <> [ "children" .= (map toJSON (filter isNode [c1,c2])) ]
    where
      isNode :: C.SemTree -> Bool
      isNode s = case s of
        C.Leaf -> False
        _ -> True
      serializeSemNode :: Maybe C.SemNode -> [Pair]
      serializeSemNode s = case s of
        Nothing -> [ "expr" .= Null, "type" .= Null, "value" .= Null ]
        Just (C.SemNode expr ty v) -> [ "expr" .= show expr, "type" .= show ty, "value" .= show v ]

instance ToJSON C.SynTree where
  toEncoding = genericToEncoding defaultOptions