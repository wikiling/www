{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Service.Settings
  ( SiteConfig(..)
  )
where

import Data.Text ( Text )
import Data.Time.Clock ( UTCTime )
import GHC.Generics ( Generic )

data SiteConfig = SiteConfig {
  environment     :: !Text
  , version       :: !Text
  , adminUsername :: !Text
  , adminPasswd   :: !Text
} deriving (Generic, Show)
