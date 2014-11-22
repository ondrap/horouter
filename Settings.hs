module Settings (
    MainSettings(..)
) where
    
import Data.UUID
import Data.Time.Clock
    
data MainSettings = MainSettings {
        msetIndex :: Int
      , msetPort :: Int
      , msetUser :: String
      , msetPassword :: String
      , msetUUID :: String
      , msetStart :: UTCTime
    }
