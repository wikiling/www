{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module App
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
import GHC.Generics
import Logger (LogMessage (..))
import Models (SyntaxTree (..), LF, transVP)
import Prelude.Compat
import Servant (Application, Capture, Context, Handler, JSON, Post, ReqBody, ServerT, err401, hoistServerWithContext, serveWithContext, throwError, (:>))
import Settings (SiteConfig (..))
import System.Log.FastLogger
  ( LoggerSet,
    ToLogStr,
    flushLogStr,
    pushLogStrLn,
    toLogStr,
  )
import Prelude ()

type FragmentAPI = "fragments" :> Capture "fragmentId" String :> ReqBody '[JSON] SyntaxTree :> Post '[JSON] FragmentHandlerResp

data AppCtx = AppCtx
  { _getConfig :: SiteConfig,
    _getLogger :: LoggerSet
  }

encodeTreeToText :: SyntaxTree -> Text
encodeTreeToText = toStrict . toLazyText . JSONText.encodeToTextBuilder . JSON.toJSON

data FragmentHandlerResp = FragmentHandlerResp
  { syntaxTree :: !SyntaxTree,
    lf :: !LF
  }
  deriving (Show, Generic)

instance JSON.ToJSON FragmentHandlerResp where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

fragmentHandler :: String -> SyntaxTree -> AppM FragmentHandlerResp
fragmentHandler fragmentId syntaxTree = do
  logset <- asks _getLogger
  tstamp <- liftIO getCurrentTime
  config <- asks _getConfig

  let logMsg =
        LogMessage
          { message = "Syntax tree: " <> encodeTreeToText syntaxTree,
            timestamp = tstamp,
            level = "info",
            lversion = version config,
            lenvironment = environment config
          }

  liftIO $ pushLogStrLn logset $ toLogStr logMsg

  pure $ FragmentHandlerResp {
    syntaxTree = syntaxTree,
    lf = transVP syntaxTree
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