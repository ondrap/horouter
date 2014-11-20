module PrioQueueHeap (
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
) where

-- Prioritni fronta postavena na Data.Heap
import Prelude hiding(lookup)
import Data.IORef
import qualified Data.PSQueue as PQ
import Control.Applicative ((<$>))

data Record a = Record a Int
instance Eq a => Eq (Record a) where
    (Record a _) == (Record b _) = a == b
instance Ord a => Ord (Record a) where
    (Record a _) `compare` (Record b _) = a `compare` b
    
recLimit :: Record a -> Int
recLimit (Record _ limit) = limit

data Queue v = Queue (IORef (PQ.PSQ (Record v) Int))
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
                resqueue = PQ.insert (PQ.key binding) (incPrio binding) nq
            in
                (resqueue, Just $ Item iq (dval $ PQ.key binding))
    where
        dval (Record a _) = a
        incPrio binding
            | priority + 1 >= limit = sentinelPriority
            | True                  = priority + 1
            where
                priority = PQ.prio binding
                limit = recLimit $ PQ.key binding
            
-- | Update priority of item with (-1) (resort in the queue)
itemMinus1 :: (Ord v) => Item v -> IO ()
itemMinus1 (Item (Queue ioref) k) = atomicModifyIORef' ioref $ \q -> 
    (PQ.adjustWithKey decPriority (Record k 0) q, ())
    where
        decPriority (Record _ limit) priority 
            | priority - 1 < sentinelPriority = limit - 1
            | True                            = priority - 1

-- | Insert new element into queue with given priority
insert :: (Ord v) => (Int, v) -> Int -> Queue v -> IO (Item v)
insert (p, k) limit iq@(Queue ioref) = atomicModifyIORef' ioref $ \q -> (PQ.insert (Record k limit) p q, Item iq k)

-- | Find a priority and priority limit of element
lookup :: (Ord v) => v -> Queue v 
    -> IO (Maybe (Int, Int)) -- ^ (current priority, maximum priority)
lookup k (Queue ioref) = do
    pq <- readIORef ioref
    case mlookup (Record k 0) pq of
         Nothing -> return Nothing
         Just b -> return $ toPair b
    where
        toPair b = return (PQ.prio b, recLimit $ PQ.key b)
        mlookup k q
            | null bindings = Nothing
            | True          = Just $ head bindings
            where
                bindings = filter (\b -> PQ.key b == k) $ PQ.toList q

-- | Adjust limit of an item
adjustLimit :: (Ord v) => v  -- ^ Item for which to adjust the limit
    -> Int      -- ^ New limit
    -> Queue v 
    -> IO Int   -- ^ Old limit (0 if key was not found)
adjustLimit k newlimit (Queue ioref) = atomicModifyIORef ioref $ \q ->
    case mlookup (Record k 0) q of
         Nothing -> (q, 0)
         Just binding ->
            case () of 
                _| oldlimit == newlimit -> (q, oldlimit)
                 | otherwise ->
                        let
                            deletedq = PQ.delete (Record k 0) q
                            newpriority = limitPrio newlimit $ delimitPrio oldlimit priority
                            newqueue = PQ.insert (Record k newlimit) newpriority deletedq
                        in
                            (newqueue, oldlimit)
            where
                priority = PQ.prio binding
                oldlimit = recLimit $ PQ.key binding
                -- Recompute priority so that it would adhere to new sentinel
                limitPrio limit priority
                    | priority >= limit = sentinelPriority + (priority - limit)
                    | True              = priority
                delimitPrio limit priority
                    | priority >= sentinelPriority = limit + (priority - sentinelPriority)
                    | True                         = priority
    where
        mlookup k q
            | null bindings = Nothing
            | True          = Just $ head bindings
            where
                bindings = filter (\b -> PQ.key b == k) $ PQ.toList q
                
-- | Delete element from queue
delete :: (Ord v) => v -> Queue v -> IO ()
delete k (Queue ioref) = atomicModifyIORef' ioref $ \q -> (PQ.delete (Record k 0) q, ())

size :: (Ord v) => Queue v -> IO Int
size (Queue ioref) = PQ.size <$> readIORef ioref
