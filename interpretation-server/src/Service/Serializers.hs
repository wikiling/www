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
import qualified Interpreter.Compose as C
import Compiler.Pretty

parseConstituencyNode :: Object -> Parser C.ConstituencyTree
parseConstituencyNode obj = do
  label <- obj .: "label"
  pos <- obj .: "id"
  cs <- obj .:? "children"
  (c1, c2) <- parseConstituencyChildren cs
  pure $ C.Node (C.CLabel label pos) c1 c2

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
  toJSON (C.Node s c1 c2) = object $ (serializeSemLabel s) <> [ "children" .= (map toJSON (filter isNode [c1,c2])) ]
    where
      isNode :: C.SemanticTree -> Bool
      isNode s = case s of
        C.Leaf -> False
        _ -> True
      
      serializeSemLabel :: C.SemanticLabel -> [Pair]
      serializeSemLabel (sl, el, cl) = (serializeEvaluationLabel sl)
                                    <> (serializeTypeCheckedExprLabel el)
                                    <> (serializeConstituencyLabel cl)

      serializeEvaluationLabel :: Maybe C.EvaluatedExpr -> [Pair]
      serializeEvaluationLabel sl = case sl of
        Nothing -> [ "value" .= Null, "error" .= Null ]
        Just (Right v) -> [ "value" .= show v, "valuationError" .= Null ]
        Just (Left err) -> [ "value" .= Null, "valuationError" .= show err ]
                            
      serializeTypeCheckedExprLabel :: Maybe C.TypeCheckedExpr -> [Pair]
      serializeTypeCheckedExprLabel el = case el of
        Nothing -> [ "expr" .= Null, "type" .= Null ]
        Just te -> case te of
          C.TypedExpr e t -> [ "expr" .= show e, "type" .= show t, "typeError" .= Null ]
          C.UntypedExpr e err -> [ "expr" .= show e, "type" .= Null, "typeError" .= show err ]

      serializeConstituencyLabel :: C.ConstituencyLabel -> [Pair]
      serializeConstituencyLabel (C.CLabel l pos) = ["syntaxLabel" .= l, "id" .= pos ]

instance ToJSON C.ConstituencyLabel where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON C.ConstituencyTree where
  toEncoding = genericToEncoding defaultOptions