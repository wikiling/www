{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module App
  ( mkApp
  )
where

import Prelude ()
import Prelude.Compat

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson
import Data.Proxy
import Data.Text
import Data.Time.Clock ( UTCTime, getCurrentTime )
import GHC.Generics
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Servant (ServerT, Handler, Context, Application, Capture, (:>), ReqBody, Post, JSON, throwError, err401, serveWithContext, hoistServerWithContext)
import System.Log.FastLogger ( LoggerSet
                             , toLogStr
                             , flushLogStr
                             , pushLogStrLn )

import Logger (LogMessage(..))
import Settings (SiteConfig(..))
import Models (SyntaxTree(..))

data AppCtx = AppCtx {
  _getConfig :: SiteConfig,
  _getLogger :: LoggerSet
}

type FragmentAPI = "fragments" :> Capture "fragmentId" String :> ReqBody '[JSON] SyntaxTree :> Post '[JSON] LogMessage

fragmentHandler :: String -> SyntaxTree -> AppM LogMessage
fragmentHandler fragmentId syntaxTree = do
    config <- asks _getConfig
    logset <- asks _getLogger
    tstamp <- liftIO getCurrentTime

    let logMsg = LogMessage { message = "Syntax tree: " <> position syntaxTree
                            , timestamp = tstamp
                            , level = "info"
                            , lversion = version config
                            , lenvironment = environment config
                            }
    -- emit log message
    liftIO $ pushLogStrLn logset $ toLogStr logMsg
    -- return handler result (for simplicity, result is a LogMessage)
    pure logMsg
fragmentHandler _ = throwError err401

fragmentApi :: Proxy FragmentAPI
fragmentApi = Proxy

type AppM = ReaderT AppCtx Handler

fragmentServer :: ServerT FragmentAPI AppM
fragmentServer = fragmentHandler

mkApp :: Context '[] -> AppCtx -> Application
mkApp cfg ctx =
  serveWithContext cfg $
    hoistServerWithContext fragmentApi
      runReaderT ctx fragmentServer