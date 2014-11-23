module Settings (
    MainSettings(..)
  , defaultMainSettings
) where
    
import Data.Time.Clock
import Data.Time.Calendar
import Network.Nats (NatsHost(..))
    
data MainSettings = MainSettings {
        msetIndex :: Int
      , msetPort :: Int

      , msetNats :: [NatsHost]
      
      , msetStartDelay :: Int
      , msetPruneInterval :: Int
      , msetDropletStaleTime :: Int
      
      , msetUUID :: String
      , msetStart :: UTCTime
    }

seconds :: Int
seconds = 1000000
    
defaultMainSettings :: MainSettings
defaultMainSettings = MainSettings {
        msetIndex = 0
      , msetPort = 2222
      , msetNats = [(NatsHost "localhost" 4222 "" "")]
      , msetStartDelay = 5 * seconds
      , msetPruneInterval = 15 * seconds
      , msetDropletStaleTime = 120 * seconds
      
      , msetUUID = "empty"
      , msetStart = UTCTime (fromGregorian 1970 1 1) 0
    }
