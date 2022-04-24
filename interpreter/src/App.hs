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
import Data.Aeson
import Data.Proxy
import Data.Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics
import Logger (LogMessage (..))
import Models (SyntaxTree (..))
import Prelude.Compat
import Servant (Application, Capture, Context, Handler, JSON, Post, ReqBody, ServerT, err401, hoistServerWithContext, serveWithContext, throwError, (:>))
import Settings (SiteConfig (..))
import System.Log.FastLogger
  ( LoggerSet,
    flushLogStr,
    pushLogStrLn,
    toLogStr,
  )
import Prelude ()

type FragmentAPI = "fragments" :> Capture "fragmentId" String :> ReqBody '[JSON] SyntaxTree :> Post '[JSON] LogMessage

data AppCtx = AppCtx
  { _getConfig :: SiteConfig,
    _getLogger :: LoggerSet
  }

fragmentHandler :: String -> SyntaxTree -> AppM LogMessage
fragmentHandler fragmentId syntaxTree = do
  config <- asks _getConfig
  logset <- asks _getLogger
  tstamp <- liftIO getCurrentTime

  let logMsg =
        LogMessage
          { message = "Syntax tree: " <> position syntaxTree,
            timestamp = tstamp,
            level = "info",
            lversion = version config,
            lenvironment = environment config
          }
  -- emit log message
  liftIO $ pushLogStrLn logset $ toLogStr logMsg
  -- return handler result (for simplicity, result is a LogMessage)
  pure logMsg

-- fragmentHandler _ _ = throwError err401

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