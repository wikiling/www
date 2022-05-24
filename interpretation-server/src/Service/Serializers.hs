{-# LANGUAGE OverloadedStrings #-}

module Service.Serializers where

import Control.Applicative ((<|>))
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Vector ((!))

import qualified Interpreter.Fragment as F
import qualified Interpreter.Composition as C
import Compiler.Pretty

parseTok :: Object -> Parser C.SynTree
parseTok obj = do
  tok <- obj .: "token"
  pure $ C.Node tok C.Leaf C.Leaf

parsePos :: Object -> Parser C.SynTree
parsePos obj = do
  pos <- obj .: "pos"
  cs <- obj .: "children"
  (c1, c2) <- parseChildren cs
  pure $ C.Node pos c1 c2

parseChildren :: Array -> Parser (C.SynTree, C.SynTree)
parseChildren cs = case length cs of
  0 -> pure $ (C.Leaf, C.Leaf)
  1 -> pure $ (parseChild 0, C.Leaf)
  2 -> pure $ (parseChild 0, parseChild 1)
  _ -> fail "must be binary tree"
  where
    parseChild i = case ((parseMaybe parseJSON $ cs!i)) of
      Nothing -> C.Leaf
      Just s  -> s

instance FromJSON C.SynTree where
  parseJSON = withObject "SyntaxTree" $ \obj ->
    parseTok obj <|> parsePos obj

instance ToJSON C.SemTree where
  toJSON t@(C.Node _ c1 c2) = object $ (node t) <> [ "children" .= (map node [c1,c2]) ]
    where
      node :: C.SemTree -> [Pair]
      node (C.Node s _ _) = case s of
        Nothing -> [ "expr" .= Null, "type" .= Null, "value" .= Null ]
        Just (C.SemNode expr ty v) -> [ "expr" .= show expr, "type" .= show ty, "value" .= show v ]

instance ToJSON C.SynTree where
  toEncoding = genericToEncoding defaultOptions