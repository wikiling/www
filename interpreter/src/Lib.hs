{-# LANGUAGE DataKinds #-}

module Lib
  ( startApp,
    app,
  )
where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Log.FastLogger ( ToLogStr(..)
                             , LoggerSet
                             , defaultBufSize
                             , newStdoutLoggerSet
                             , flushLogStr
                             , pushLogStrLn )

import Api (FragmentAPI)
import App (fragmentServer)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api fragmentServer

api :: Proxy FragmentAPI
api = Proxy
