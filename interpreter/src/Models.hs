{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Models
  ( SyntaxTree (..),
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson.Parser
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Text (Text)
import Data.Time.Calendar
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

type Id = String

type Pos = String

type Lexeme = String

data SyntaxTree = Node Id Pos [SyntaxTree] | Leaf Id Lexeme deriving (Show)

-- data SyntaxTree = Node {meta :: Text, children :: [SyntaxTree]} deriving (Generic)

instance FromJSON SyntaxTree where
  parseJSON = withObject "SyntaxTree" $ \obj -> do
    id <- obj .: "id"
    pos <- obj .: "pos"
    lexeme <- obj .: "lexeme"
    children <- obj .: "children"

    if children
      then return Node id pos (decode children)
      else return Leaf id lexeme

{-
  parseJSON = withObject "SyntaxTree" $ \o ->
    Node <$> o .: "object"
      <*> o .: "id"
      <*> o .:? "pos"
      <*> o .:? "folderId"
      <*> o .: "type"
      <*> o .: "name"
      <*> o .: "notes"
      <*> o .: "favorite"
      <*> parseItemType o
      <*> o .: "collectionIds"
      <*> o .:? "revisionDate"
    where
      parseItemType o =
        MkLogin <$> o .: "login"
          <|> MkCard <$> o .: "card"
          <|> MkIdentity <$> o .: "identity"
          <|> MkSecureNote <$> o .: "securenote"

-}

instance ToJSON SyntaxTree
