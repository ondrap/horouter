module PrioQueue (
    Queue
  , Item
  , newQueue
  , valueOf
  , getMinAndPlus1
  , getRouteAndPlus1
  , itemMinus1
  , insert
  , delete
  , lookup
  , size
  , adjustLimit
  , toList
  , removeBy
) where

-- Prioritni fronta postavena na Data.Heap
import Prelude hiding(lookup)
import Data.IORef
import qualified Data.PSQueue as PQ
import Control.Applicative ((<$>))

-- d is additional data, it is best bound to priority because it can be easily changed
data Priority d = Priority Int Int d
instance Eq (Priority d) where
    (Priority p1 l1 _) == (Priority p2 l2 _)
        | p1 < l1 && p2 < l2 = p1 == p2
        | p1 < l1 || p2 < l2 = False
        | otherwise          = (p1 - l1) == (p2 - l2)

instance Ord (Priority d) where
    (Priority p1 l1 _) `compare` (Priority p2 l2 _)
        | p1 < l1 && p2 < l2  = p1 `compare` p2
        | p1 < l1 = LT
        | p2 < l2 = GT
        | otherwise = (p1 - l1) `compare` (p2 - l2)

data Queue v d = Queue (IORef (PQ.PSQ v (Priority d)))
data Item v d = Item (Queue v d) v d

-- | Return value stored in Item
valueOf :: Item v d -> (v, d)
valueOf (Item _ v d) = (v, d)

-- | Create new empty priority queue
newQueue :: (Ord v) => IO (Queue v d)
newQueue = Queue <$> newIORef PQ.empty

-- | Get item with min priority and update priority with (+1)
getMinAndPlus1 :: (Ord v) => Queue v d -> IO (Maybe (Item v d))
getMinAndPlus1 iq@(Queue ioref) = atomicModifyIORef' ioref $ \q ->
    case PQ.minView q of
         Just (binding, nq) ->
            let
                (Priority pr limit dta) = PQ.prio binding
                resqueue = PQ.insert (PQ.key binding) (Priority (pr+1) limit dta) nq
            in
                (resqueue, Just $ Item iq (PQ.key binding) dta)
         Nothing -> (q, Nothing)

-- | Get route by preferred item
getRouteAndPlus1 :: (Ord v) => Queue v d -> v -> IO (Maybe (Item v d))
getRouteAndPlus1 iq@(Queue ioref) k = atomicModifyIORef' ioref $ \q ->
    case PQ.lookup k q of
         Just (Priority pr limit dta) ->
            let resqueue = PQ.adjust (const $ Priority (pr+1) limit dta) k q
            in (resqueue, Just $ Item iq k dta)
         Nothing -> (q, Nothing)

-- | Update priority of item with (-1) (resort in the queue)
itemMinus1 :: (Ord v) => Item v d -> IO ()
itemMinus1 (Item (Queue ioref) k _) = atomicModifyIORef' ioref $ \q ->
    (PQ.adjust decPriority k q, ())
    where
        decPriority (Priority priority limit dt) = Priority (priority - 1) limit dt

-- | Insert new element into queue with given priority
insert :: (Ord v) => (Int, v, d) -> Int -> Queue v d -> IO ()
insert (p, k, d) limit (Queue ioref) = atomicModifyIORef' ioref $ \q -> (PQ.insert k (Priority p limit d) q, ())

-- | Find a priority and priority limit of element
lookup :: (Ord v) => v -> Queue v d
    -> IO (Maybe (Int, Int, d)) -- ^ (current priority, maximum priority)
lookup k (Queue ioref) = do
    pq <- readIORef ioref
    case PQ.lookup k pq of
         Nothing -> return Nothing
         Just (Priority prio limit dt) -> return $ Just (prio, limit, dt)

-- | Adjust limit of an item
adjustLimit :: (Ord v) => v  -- ^ Item for which to adjust the limit
    -> Int      -- ^ New limit
    -> Queue v d
    -> IO (Maybe Int)   -- ^ Old limit (Nothing if key does not exist))
adjustLimit k newlimit (Queue ioref) = atomicModifyIORef ioref $ \q ->
    case PQ.lookup k q of
         Nothing -> (q, Nothing)
         Just (Priority priority oldlimit dt) ->
            case () of
                _| oldlimit == newlimit -> (q, Just oldlimit)
                 | otherwise ->
                        let
                            newqueue = PQ.update (const $ Just (Priority priority newlimit dt)) k q
                        in
                            (newqueue, Just oldlimit)

-- | Delete element from queue
delete :: (Ord v) => v -> Queue v d -> IO ()
delete k (Queue ioref) = atomicModifyIORef' ioref $ \q -> (PQ.delete k q, ())

size :: (Ord v) => Queue v d -> IO Int
size (Queue ioref) = PQ.size <$> readIORef ioref

-- | Convert queue to list, mostly for debugging purposes
toList :: (Ord v) => Queue v d -> IO [(Int, Int, v, d)]
toList (Queue ioref) = do
    pq <- readIORef ioref
    return $ map convert $ PQ.toList pq
    where
        convert binding = (prio, limit, PQ.key binding, dta)
            where
                (Priority prio limit dta) = PQ.prio binding

removeBy :: (Ord v) => ((v, d) -> Bool) -> Queue v d -> IO ()
removeBy check (Queue ioref) = atomicModifyIORef ioref $ \q ->
    (PQ.foldl handleItem q q, ())
    where
        handleItem dq binding
            | (Priority _ _ dt) <- PQ.prio binding, check (PQ.key binding, dt) = PQ.delete (PQ.key binding) dq
            | otherwise = dq
