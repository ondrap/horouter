{-# LANGUAGE OverloadedStrings   #-}

module Settings (
    MainSettings(..)
  , defaultMainSettings
  , readSettings
  , defaultStaleInterval
  , seconds
) where

import Data.Time.Clock
import Data.Time.Calendar
import Network.Nats (NatsHost(..))
import Data.Yaml as Y
import Data.Aeson as AE
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)

data MainSettings = MainSettings {
        msetIndex :: Int
      , msetPort :: Int

      , msetNats :: [NatsHost]

      , msetPruneStaleDropletsInterval :: Int
      , msetDropletStaleThreshold :: Int

      , msetUUID :: String
      , msetStart :: UTCTime
    } deriving (Show)

instance AE.FromJSON MainSettings where
  parseJSON (AE.Object v) =
      MainSettings <$>
        v .: "index" .!= 0 <*>
        v .: "port" .!= 8888 <*>
        v .: "nats" .!= [NatsHost "localhost" 4222 "nats" "nats"] <*>
        v .: "prune_stale_droplets_interval" .!= 30 <*>
        v .: "droplet_stale_threshold" .!= 120 <*>
        pure "-" <*>
        pure (UTCTime (fromGregorian 1970 1 1) 0)
  parseJSON _ = mzero

seconds :: Int
seconds = 1000000

defaultStaleInterval :: Int
defaultStaleInterval = 120

defaultMainSettings :: MainSettings
defaultMainSettings = MainSettings {
        msetIndex = 0
      , msetPort = 2222
      , msetNats = [NatsHost "localhost" 4222 "" ""]
      , msetPruneStaleDropletsInterval = 30
      , msetDropletStaleThreshold = defaultStaleInterval

      , msetUUID = "empty"
      , msetStart = UTCTime (fromGregorian 1970 1 1) 0
    }

readSettings :: FilePath -> IO MainSettings
readSettings fname = do
    res <- Y.decodeFileEither fname
    case res of
        Right settings -> return settings
        Left err -> error (show err)
