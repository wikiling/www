module Main where
import Servant
import Logger (LogMessage)
import App (mkApp)

port :: Int
port = 8080

jsonRequestLogger :: IO Middleware
jsonRequestLogger =
  mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

main :: IO ()
main = do
  -- typically, we'd create our config from environment variables
  -- but we're going to just make one here
  let config = SiteConfig "dev" "1.0.0" "admin" "secretPassword"

  warpLogger <- jsonRequestLogger
  appLogger <- newStdoutLoggerSet defaultBufSize

  tstamp <- getCurrentTime
  myKey <- generateKey

  let lgmsg = LogMessage {
    message = "My app starting up!"
    , timestamp = tstamp
    , level = "info"
    , lversion = version config
    , lenvironment = environment config
  }
  pushLogStrLn appLogger (toLogStr lgmsg) >> flushLogStr appLogger

  let ctx = AppCtx config appLogger

      warpSettings = Warp.defaultSettings
      portSettings = Warp.setPort port warpSettings
      settings = Warp.setTimeout 55 portSettings
      cfg = EmptyContext

  Warp.runSettings settings $ warpLogger $ mkApp cfg ctx
