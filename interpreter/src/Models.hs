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
import Prelude ()

newtype SyntaxLeaf = SyntaxLeaf {lexeme :: String} deriving (Generic)

data SyntaxTree = SyntaxTree
  { position :: Text,
    partOfSpeech :: Text,
    children :: [SyntaxTree]
  }
  deriving (Generic)

instance FromJSON SyntaxTree

instance ToJSON SyntaxTree
