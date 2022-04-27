{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Models
  ( SyntaxTree (..),
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Parser
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.Aeson.Types as JSONT
import qualified Data.Attoparsec.ByteString as BS
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Text (Text)
import Data.Time.Calendar
import qualified Data.Vector as V
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Prelude.Compat
import Servant
import Servant.Types.SourceT (source)
import System.Directory
import Text.Blaze
import qualified Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8

type Id = Text

type Pos = Text

type Lexeme = Text

data SyntaxTree = Node Id Pos [SyntaxTree] | Leaf Id Lexeme
  deriving (Show, Generic, JSON.ToJSON)

{-
syntaxNodeArrayParser' :: [JSON.Value] -> Parser [SyntaxTree]
syntaxNodeArrayParser' a = case a of
  [] -> return ([] :: [SyntaxTree])
  (Object o : os) -> syntaxNodeParser o >> syntaxNodeArrayParser' os

syntaxNodeArrayParser :: JSON.Value -> Parser [SyntaxTree]
syntaxNodeArrayParser v = case v of
  (Array a) -> syntaxNodeArrayParser' (V.toList a)
-}

syntaxNodeParser :: JSON.Object -> Parser SyntaxTree
syntaxNodeParser obj =
  Leaf <$> obj .: "id" <*> obj .: "token"
    <|> Node <$> obj .: "id" <*> obj .: "pos" <*> obj .: "children"

instance FromJSON SyntaxTree where
  parseJSON = JSON.withObject "SyntaxTree" syntaxNodeParser
