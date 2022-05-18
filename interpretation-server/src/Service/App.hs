{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Service.App
  ( mkApp,
    AppCtx (..),
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as JSONText
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHComp.Generics
import Prelude.Compat
import Servant (Application, Capture, Context, Handler, JSON, Post, ReqBody, ServerT, err401, hoistServerWithContext, serveWithContext, throwError, (:>))
import System.Log.FastLogger
  ( LoggerSet,
    ToLogStr,
    flushLogStr,
    pushLogStrLn,
    toLogStr,
  )

import Service.Logger (LogMessage)
import Service.Settings (SiteConfig)
import Service.Serializers

import qualified Interpreter.Composition as Comp

type FragmentAPI = "fragments" :> Capture "fragmentId" String :> ReqBody '[JSON] Comp.SynTree :> Post '[JSON] FragmentHandlerResp

data AppCtx = AppCtx
  { _getConfig :: SiteConfig,
    _getLogger :: LoggerSet
  }

encodeTreeToText :: Comp.SynTree -> Text
encodeTreeToText = toStrict . toLazyText . JSONText.encodeToTextBuilder . JSON.toJSON

data FragmentHandlerResp = FragmentHandlerResp
  { syntaxTree :: !Comp.SynTree,
    semanticTree :: !Comp.SemTree
  }
  deriving (Show, Generic)

instance JSON.ToJSON FragmentHandlerResp where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

fragmentHandler :: String -> Comp.SynTree -> AppM FragmentHandlerResp
fragmentHandler fragmentId syntaxTree = do
  logset <- asks _getLogger
  tstamp <- liftIO getCurrentTime
  config <- asks _getConfig

  liftIO $ pushLogStrLn logset $ toLogStr LogMessage
    { message = "fragmtn: " <> fragmentId <> " Syntax tree: " <> encodeTreeToText syntaxTree,
      timestamp = tstamp,
      level = "info",
      lversion = version config,
      lenvironment = environment config
    }
  
  fragment <- liftIO $ Comp.loadFragment fragmentId
  semTree <- Comp.runComposition fragment syntaxTree

  pure $ FragmentHandlerResp {
    syntaxTree = syntaxTree,
    semTree = Comp. syntaxTree
  }

fragmentApi :: Proxy FragmentAPI
fragmentApi = Proxy

type AppM = ReaderT AppCtx Handler

fragmentServer :: ServerT FragmentAPI AppM
fragmentServer = fragmentHandler

mkApp :: Context '[] -> AppCtx -> Application
mkApp cfg ctx =
  serveWithContext fragmentApi cfg $
    hoistServerWithContext
      fragmentApi
      (Proxy :: Proxy '[])
      (flip runReaderT ctx)
      fragmentServer