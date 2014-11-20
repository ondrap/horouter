module PrioQueue (
    Queue
  , Item
  , newQueue
  , valueOf
  , getMinAndPlus1
  , itemMinus1
  , insert
  , delete
  , lookup
  , size
  , adjustLimit
  , toList
) where

-- Prioritni fronta postavena na Data.Heap
import Prelude hiding(lookup)
import Data.IORef
import qualified Data.PSQueue as PQ
import Control.Applicative ((<$>))

data Priority = Priority Int Int
instance Eq Priority where
    (Priority p1 _) == (Priority p2 _) = p1 == p2
instance Ord Priority where
    (Priority p1 l1) `compare` (Priority p2 l2)
        | p1 == p2 = l1 `compare` l2
        | True     = p1 `compare` p2

data Queue v = Queue (IORef (PQ.PSQ v Priority))
data Item v = Item (Queue v) v

-- | If the priority gets over limit, it gets upgraded to sentinelPriority,
-- so that items with priority lower than limit are served first
sentinelPriority :: Int
sentinelPriority = 1000000

-- | Return value stored in Item
valueOf :: Item v -> v
valueOf (Item _ v) = v

-- | Create new empty priority queue
newQueue :: (Ord v) => IO (Queue v)
newQueue = Queue <$> newIORef PQ.empty 

-- | Get item with min priority and update priority with (+1)
getMinAndPlus1 :: (Ord v) => Queue v -> IO (Maybe (Item v))
getMinAndPlus1 iq@(Queue ioref) = atomicModifyIORef' ioref $ \q ->
    case (PQ.minView q) of
         Nothing -> (q, Nothing)
         Just (binding, nq) ->
            let 
                resqueue = PQ.insert (PQ.key binding) (incPrio $ PQ.prio binding) nq
            in
                (resqueue, Just $ Item iq (PQ.key binding))
    where
        incPrio (Priority priority limit)
            | priority + 1 == limit = Priority sentinelPriority limit
            | True                  = Priority (priority + 1) limit
            
-- | Update priority of item with (-1) (resort in the queue)
itemMinus1 :: (Ord v) => Item v -> IO ()
itemMinus1 (Item (Queue ioref) k) = atomicModifyIORef' ioref $ \q -> 
    (PQ.adjust decPriority k q, ())
    where
        decPriority (Priority priority limit)
            | priority == sentinelPriority = Priority (limit - 1) limit
            | True                         = Priority (priority - 1) limit

-- | Insert new element into queue with given priority
insert :: (Ord v) => (Int, v) -> Int -> Queue v -> IO (Item v)
insert (p, k) limit iq@(Queue ioref) = atomicModifyIORef' ioref $ \q -> (PQ.insert k (Priority p limit) q, Item iq k)

-- | Find a priority and priority limit of element
lookup :: (Ord v) => v -> Queue v 
    -> IO (Maybe (Int, Int)) -- ^ (current priority, maximum priority)
lookup k (Queue ioref) = do
    pq <- readIORef ioref
    case PQ.lookup k pq of
         Nothing -> return Nothing
         Just (Priority prio limit) -> return $ Just (prio, limit)

-- | Adjust limit of an item
adjustLimit :: (Ord v) => v  -- ^ Item for which to adjust the limit
    -> Int      -- ^ New limit
    -> Queue v 
    -> IO (Maybe Int)   -- ^ Old limit (Nothing if key does not exist))
adjustLimit k newlimit (Queue ioref) = atomicModifyIORef ioref $ \q ->
    case PQ.lookup k q of
         Nothing -> (q, Nothing)
         Just (Priority oldpriority oldlimit) ->
            case () of 
                _| oldlimit == newlimit -> (q, Just oldlimit)
                 | otherwise ->
                        let
                            deletedq = PQ.delete k q
                            newpriority = limitPrio newlimit $ delimitPrio oldlimit oldpriority
                            newqueue = PQ.insert k (Priority newpriority newlimit) deletedq
                        in
                            (newqueue, Just oldlimit)
            where
                -- Recompute priority so that it would adhere to new sentinel
                limitPrio limit priority
                    | priority >= limit = sentinelPriority + (priority - limit)
                    | True              = priority
                delimitPrio limit priority
                    | priority >= sentinelPriority = limit + (priority - sentinelPriority)
                    | True                         = priority
                
-- | Delete element from queue
delete :: (Ord v) => v -> Queue v -> IO ()
delete k (Queue ioref) = atomicModifyIORef' ioref $ \q -> (PQ.delete k q, ())

size :: (Ord v) => Queue v -> IO Int
size (Queue ioref) = PQ.size <$> readIORef ioref

-- | Convert queue to list, mostly for debugging purposes
toList :: (Ord v) => Queue v -> IO [(Int, Int, v)]
toList (Queue ioref) = do
    pq <- readIORef ioref
    return $ map convert $ PQ.toList pq
    where 
        convert binding = (prio, limit, PQ.key binding)
            where
                (Priority prio limit) = PQ.prio binding
