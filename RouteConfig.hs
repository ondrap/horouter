{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, DeriveDataTypeable #-}

module RouteConfig (
    RouteConfig
  , Route(..)
  , RouteInfo(..)
  , newRouteConfig
  , routeAdd
  , routeDel
  , routeSize
  , pruneStaleRoutes
  , withBestRoute 
  , RouteException(..)
) where
    
import qualified PrioQueue as PQ
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Map.Strict as M
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS
import Control.Exception (bracket, catch, throwIO, SomeException, Exception(..))
import qualified Data.CaseInsensitive as CI
import qualified Network.Socket as N
import Data.Typeable
import qualified Control.Concurrent.MSemN as SEM
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Monad (forever, foldM)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Control.Monad.Trans.Class (lift)

import Settings

-- | Information identifying the route
data Route = Route {
        routeHost :: BS.ByteString 
      , routePort :: Int 
    } deriving (Eq, Ord)
    
-- | Additional information for the route
data RouteInfo = RouteInfo {
        routeSockAddr :: N.SockAddr 
      , routeAppId :: BS.ByteString
      , routeExpireTime :: UTCTime
    }
    
-- Allow N.SockAddr to be undefined for some operations
instance Show Route where
    show (Route {routeHost, routePort}) = (BS.unpack routeHost) ++ ":" ++ (show routePort)

data HostRoute = HostRoute {
        hostRoutes :: PQ.Queue Route RouteInfo
      , hostSemaphore :: SEM.MSemN Int
    }
    
type RouteMap = MVar (M.Map (CI.CI BS.ByteString) HostRoute)
data RouteConfig = RouteConfig {
        routeMap :: RouteMap
    }

newRouteConfig :: IO RouteConfig
newRouteConfig = RouteConfig <$> newMVar M.empty

routeAdd :: RouteConfig 
    -> BS.ByteString  -- ^ Domain mapping
    -> Route          -- ^ IP address:port of instance
    -> RouteInfo    -- ^ Additional 'mutable' information for route
    -> Int            -- ^ Maximum parallel operations on this route
    -> IO ()
routeAdd _ _ _ _ limit
    | limit <= 0 = putStrLn "Incorrect parallelism limit"
routeAdd (RouteConfig {routeMap}) uri route rtinfo limit = modifyMVar_ routeMap $ \rmap -> do
    putStrLn $ "Registering web service: " ++ (show uri) ++ " -> " ++ (show route) ++ " parallel limit: " ++ (show limit)
    case (M.lookup iuri rmap) of
        Just hroute -> do
            let hq = hostRoutes hroute
                hsem = hostSemaphore hroute
            moldlimit <- PQ.adjustLimit route limit hq
            case moldlimit of
                    Nothing -> do
                        SEM.signal hsem limit
                        PQ.insert (0, route, rtinfo) limit hq
                        return ()
                    Just oldlimit -> do
                        -- Update limit of an item if it is different
                        case () of
                           _| limit > oldlimit -> SEM.signal hsem (limit - oldlimit)
                            | limit < oldlimit -> (forkIO $ SEM.wait hsem (oldlimit - limit)) >> return ()
                            | otherwise        -> return ()
            return rmap
        Nothing -> do
            q <- PQ.newQueue
            PQ.insert (0, route, rtinfo) limit q
            msem <- SEM.new limit
            return $ M.insert iuri (HostRoute q msem) rmap
    where
        iuri = CI.mk uri

-- debug uri rmap = do
--     case M.lookup uri rmap of
--         Nothing -> return ()
--         (Just hroute) -> do 
--             dlist <- PQ.toList (hostRoutes hroute)
--             putStrLn $ show dlist
--             avail <- SEM.peekAvail (hostSemaphore hroute)
--             putStrLn $ show avail
        
-- | Count number of members in route
routeSize :: RouteConfig -> BS.ByteString -> IO Int
routeSize (RouteConfig {routeMap}) uri = do
    rmap <- readMVar routeMap
    case M.lookup (CI.mk uri) rmap of
         Nothing -> return 0
         Just hroute -> PQ.size $ hostRoutes hroute
        
routeDel :: RouteConfig 
    -> BS.ByteString    -- ^ Domain mapping
    -> Route            -- ^ IP address:port of instance
    -> IO ()
routeDel (RouteConfig {routeMap}) uri route = modifyMVar_ routeMap $ \rmap -> do
    putStrLn $ "Unregistering web service: " ++ (show uri) ++ " -> " ++ (show route)
    newmap <- runMaybeT $ do
        hroute <- MaybeT (return $ M.lookup iuri rmap)
        (_, limit, _) <- MaybeT $ PQ.lookup route (hostRoutes hroute)
        -- Remove from mapping
        lift $ PQ.delete route (hostRoutes hroute)
        _ <- lift $ forkIO $ SEM.wait (hostSemaphore hroute) limit
        lift $ updateEmptyMapping iuri hroute rmap
    return $ maybe rmap id newmap
    where
        iuri = CI.mk uri

   
-- | Remove entry from routeMap if there are no remaining routes
updateEmptyMapping :: CI.CI BS.ByteString -> HostRoute 
                        -> M.Map (CI.CI BS.ByteString) HostRoute 
                        -> IO (M.Map (CI.CI BS.ByteString) HostRoute)
updateEmptyMapping iuri hroute rmap = do
    size <- PQ.size $ hostRoutes hroute
    case size of
        0 -> do
            -- Add something to the semaphore, so that things on the semaphore will
            -- eventually get through
            SEM.signal (hostSemaphore hroute) 1
            return $ M.delete iuri rmap
        _ -> do
            return rmap
    
        
pruneStaleRoutes :: MainSettings -> RouteConfig -> IO ()
pruneStaleRoutes settings rconf@(RouteConfig {routeMap=mRouteMap}) = forever $ do
    now <- getCurrentTime
    threadDelay $ (msetPruneStaleDropletsInterval settings) * seconds
    modifyMVar_ mRouteMap $ \rmap -> do
        foldM (removeStale now) rmap (M.toList rmap)
        
    where
        removeStale now rmap (uri, hroute) = do
            PQ.removeBy (\(rt, dta) -> (routeExpireTime dta) < now) (hostRoutes hroute)
            updateEmptyMapping uri hroute rmap
        
data RouteException = RouteException Route SomeException
    deriving (Show, Typeable)
instance Exception RouteException

-- | Find HostRoute entry in routeMap, call notFound if not found
findHostRoute :: RouteConfig -> BS.ByteString -> IO a -> (HostRoute -> IO a) -> IO a
findHostRoute (RouteConfig {routeMap}) uri notFound routeFound = do
    rmap <- readMVar routeMap
    case (M.lookup (CI.mk uri) rmap) of
        Nothing -> notFound
        Just x -> routeFound x

-- | Run some code while ensuring correct counting of connections in queue
withBestRoute :: RouteConfig       -- ^ Shared routing information
    -> BS.ByteString               -- ^ Hostname to route
    -> Maybe Route -- ^ Preferred hostname/port
    -> IO a                        -- ^ Call if no route found
    -> ((Route, RouteInfo) -> IO a)             -- ^ Call if route found
    -> IO a                        -- ^ Result
    
-- Try to route according to preferred host/port; if none is found, 
-- failback to leastconn.
-- We do not count the persistent connections against total connections semaphore,
-- as this is a sum of connections to each agent; number of persistent connections
-- could exceed a maximum for each agent and the semaphore wouldn't behave correctly
withBestRoute rconf uri (Just prefroute) notFound routeFound = do
    findHostRoute rconf uri notFound $ \(HostRoute {hostRoutes}) -> do
        bracket
            (PQ.getRouteAndPlus1 hostRoutes prefroute)
            (maybe (return ()) PQ.itemMinus1)
            (\item' -> case item' of
                        Nothing ->
                            withBestRoute rconf uri Nothing notFound routeFound -- Failback
                        Just item ->
                            routeFound (PQ.valueOf item) `catch` (throwIO . RouteException prefroute)
            )
    
-- Route with least_conn
withBestRoute rconf uri Nothing notFound routeFound = do
    findHostRoute rconf uri notFound $ \(HostRoute {hostRoutes, hostSemaphore}) -> do
        SEM.with hostSemaphore 1 $ 
            bracket
                (PQ.getMinAndPlus1 hostRoutes)
                (maybe (return ()) PQ.itemMinus1)
                (maybe notFound $
                    \item -> let (route, _) = PQ.valueOf item
                             in routeFound (PQ.valueOf item) `catch` (throwIO . RouteException route)
                )
