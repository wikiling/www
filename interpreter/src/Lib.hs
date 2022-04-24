{-# LANGUAGE DataKinds #-}

module Lib where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Log.FastLogger
  ( LoggerSet,
    ToLogStr (..),
    defaultBufSize,
    flushLogStr,
    newStdoutLoggerSet,
    pushLogStrLn,
  )

-- startApp :: IO ()
-- startApp = run 8080 app

-- app :: Application
-- app = serve api fragmentServer

-- api :: Proxy FragmentAPI
-- api = Proxy
