{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Logger
  ( LogMessage (..),
    DebugMessage (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import System.Log.FastLogger
  ( LoggerSet,
    ToLogStr (..),
    flushLogStr,
    newStdoutLoggerSet,
    pushLogStrLn,
  )

data LogMessage = LogMessage
  { message :: !Text,
    timestamp :: !UTCTime,
    level :: !Text,
    lversion :: !Text,
    lenvironment :: !Text
  }
  deriving (Eq, Show, Generic)

data DebugMessage = DebugMessage
  {m :: !Text}
  deriving (Eq, Show, Generic)

instance FromJSON LogMessage

instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

instance ToJSON DebugMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr DebugMessage where
  toLogStr = toLogStr . encode