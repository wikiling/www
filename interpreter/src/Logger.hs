{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Logger
  ( LogMessage(..)
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)
import Data.Aeson
import System.Log.FastLogger ( ToLogStr(..)
                             , LoggerSet
                             , defaultBufSize
                             , newStdoutLoggerSet
                             , flushLogStr
                             , pushLogStrLn )

data LogMessage = LogMessage {
  message        :: !Text
  , timestamp    :: !UTCTime
  , level        :: !Text
  , lversion     :: !Text
  , lenvironment :: !Text
} deriving (Eq, Show, Generic)

instance FromJSON LogMessage
instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode
