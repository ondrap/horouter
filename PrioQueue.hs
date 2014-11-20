-- module PrioQueue
-- (
-- ) where
    
import Control.Concurrent.MVar
import Data.IORef

data Item v = Item {
        itemValue :: v
      , itemNext :: IORef (Maybe (Priority v))
      , itemPrev :: IORef (Maybe (Priority v))
      , itemPriority :: IORef (Maybe (Priority v))
    }

data Priority v = Priority {
        priPriority :: Int
      , priFirstItem :: IORef (Item v)
      , priNext :: IORef (Maybe (Priority v))
      , priPrev :: IORef (Maybe (Priority v))
      , priHead :: Head v
    }
    
data Head v = Head (IORef (Maybe (Priority v)))
data Queue v = MVar (Head v)
    
    
    
main = putStrLn "Hello"
