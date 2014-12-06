module Settings (
    MainSettings(..)
  , defaultMainSettings
  , defaultStaleInterval
  , seconds
) where

import Data.Time.Clock
import Data.Time.Calendar
import Network.Nats (NatsHost(..))

data MainSettings = MainSettings {
        msetIndex :: Int
      , msetPort :: Int

      , msetNats :: [NatsHost]

      , msetPruneStaleDropletsInterval :: Int
      , msetDropletStaleThreshold :: Int

      , msetUUID :: String
      , msetStart :: UTCTime
    }

seconds :: Int
seconds = 1000000

defaultStaleInterval :: Int
defaultStaleInterval = 120

defaultMainSettings :: MainSettings
defaultMainSettings = MainSettings {
        msetIndex = 0
      , msetPort = 2222
      , msetNats = [(NatsHost "localhost" 4222 "" "")]
      , msetPruneStaleDropletsInterval = 30
      , msetDropletStaleThreshold = defaultStaleInterval

      , msetUUID = "empty"
      , msetStart = UTCTime (fromGregorian 1970 1 1) 0
    }
