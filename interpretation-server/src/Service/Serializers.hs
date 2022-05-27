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

parseConstituencyNode :: Object -> Parser C.ConstituencyTree
parseConstituencyNode obj = do
  label <- obj .: "label"
  pos <- obj .: "id"
  cs <- obj .:? "children"
  (c1, c2) <- parseConstituencyChildren cs
  pure $ C.Node (C.CNodeLabel label pos) c1 c2

parseConstituencyChildren :: Maybe Array -> Parser (C.ConstituencyTree, C.ConstituencyTree)
parseConstituencyChildren Nothing = pure $ (C.Leaf, C.Leaf)
parseConstituencyChildren (Just cs) = case length cs of
  0 -> pure $ (C.Leaf, C.Leaf)
  1 -> pure $ (parseChild 0, C.Leaf)
  2 -> pure $ (parseChild 0, parseChild 1)
  _ -> fail "must be binary tree"
  where
    parseChild :: Int -> C.ConstituencyTree
    parseChild i = case ((parseMaybe parseJSON $ cs!i) :: Maybe C.ConstituencyTree) of
      Nothing -> C.Leaf
      Just s  -> s

instance FromJSON C.ConstituencyTree where
  parseJSON = withObject "SyntaxTree" parseConstituencyNode

instance ToJSON C.SemanticTree where
  toJSON (C.Node s c1 c2) = object $ (serializeSemNodeLabel s) <> [ "children" .= (map toJSON (filter isNode [c1,c2])) ]
    where
      isNode :: C.SemanticTree -> Bool
      isNode s = case s of
        C.Leaf -> False
        _ -> True
      serializeSemNodeLabel :: C.SemanticNodeLabel -> [Pair]
      serializeSemNodeLabel s = case s of
        C.EmptySemNode cnl -> [ "expr" .= Null, "type" .= Null, "value" .= Null ]
                              <> serializeConstituencyNodeLabel cnl
        C.EvaluatedSemNode expr ty v cnl -> [ "expr" .= show expr, "type" .= show ty, "value" .= show v ]
                                            <> serializeConstituencyNodeLabel cnl
      serializeConstituencyNodeLabel :: C.ConstituencyNodeLabel -> [Pair]
      serializeConstituencyNodeLabel (C.CNodeLabel l pos) = ["constituencyLabel" .= l, "id" .= pos ]

instance ToJSON C.ConstituencyNodeLabel where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON C.ConstituencyTree where
  toEncoding = genericToEncoding defaultOptions