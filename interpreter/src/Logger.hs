{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Logger
  ( LogMessage (..),
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

instance FromJSON LogMessage

instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode
