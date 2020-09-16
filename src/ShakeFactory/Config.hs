{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The sf.dhall type
module ShakeFactory.Config
  ( -- * Main data types
    SoftwareFactoryContainer (..),
    SoftwareFactoryConfig (..),

    -- * Utility function
    loadConfig,
  )
where

import Dhall (FromDhall, Generic, auto, inputFile)

data SoftwareFactoryContainer = SoftwareFactoryContainer
  { imageRef :: String,
    builderRef :: String
  }
  deriving (Show, Generic, FromDhall)

data SoftwareFactoryConfig = SoftwareFactoryConfig
  { name :: String,
    container :: SoftwareFactoryContainer
  }
  deriving (Show, Generic, FromDhall)

loadConfig :: FilePath -> IO SoftwareFactoryConfig
loadConfig = inputFile auto
