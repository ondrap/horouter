{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, DeriveDataTypeable #-}

module RouteConfig (
    RouteConfig
  , Route(..)
  , RouteInfo(..)
  , newRouteConfig
  , routeAdd
  , routeDel
  , routeSize
  , purgeStaleRoutes
  , withBestRoute 
  , RouteException(..)
  , defaultStaleInterval
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
import Control.Monad (forever)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Control.Monad.Trans.Class (lift)

defaultStaleInterval :: Int
defaultStaleInterval = 120

-- | Information identifying the route
data Route = Route {
        routeHost :: BS.ByteString 
      , routePort :: Int 
    } deriving (Eq, Ord)
    
-- | Additional information for the route
data RouteInfo = RouteInfo {
        routeSockAddr :: N.SockAddr 
      , routeAppId :: BS.ByteString
      , routeRegTime :: UTCTime
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

-- TODO: pruning po 2 minutach
    
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
routeAdd (RouteConfig {routeMap}) uri addr rtinfo limit = modifyMVar_ routeMap $ \rmap -> do
    putStrLn $ "Registering web service: " ++ (show uri) ++ " -> " ++ (show addr) ++ " parallel limit: " ++ (show limit)
    case (M.lookup iuri rmap) of
        Just hroute -> do
            let hq = hostRoutes hroute
                hsem = hostSemaphore hroute
            moldlimit <- PQ.adjustLimit addr limit hq
            case moldlimit of
                    Nothing -> do
                        SEM.signal hsem limit
                        PQ.insert (0, addr, rtinfo) limit hq
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
            PQ.insert (0, addr, rtinfo) limit q
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
        lift $ deleteRouteFromMapping iuri route hroute limit rmap
    return $ maybe rmap id newmap
    where
        iuri = CI.mk uri

   
deleteRouteFromMapping :: CI.CI BS.ByteString -> Route -> HostRoute 
                        -> Int -> M.Map (CI.CI BS.ByteString) HostRoute 
                        -> IO (M.Map (CI.CI BS.ByteString) HostRoute)
deleteRouteFromMapping iuri route hroute limit rmap = do
    PQ.delete route (hostRoutes hroute)
    _ <- forkIO $ SEM.wait (hostSemaphore hroute) limit
    
    size <- PQ.size $ hostRoutes hroute
    case size of
        0 -> do
            -- Add something to the semaphore, so that things on the semaphore will
            -- eventually get through
            SEM.signal (hostSemaphore hroute) 1
            return $ M.delete iuri rmap
        _ -> do
            return rmap
    
        
purgeStaleRoutes :: RouteConfig -> IO ()
purgeStaleRoutes rconf@(RouteConfig {routeMap=mRouteMap}) = forever $ do
    now <- getCurrentTime
    threadDelay (defaultStaleInterval * 1000000)
--     modifyMVar_ mRoutMap $ \rmap -> do
--         newmap <- foldM (removeStale now) rmap (M.toList rmap)
-- 
-- removeStale now rmap (uri, hostRoutes) = do
     
            
        
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
    -> Maybe (BS.ByteString, Int)  -- ^ Preferred hostname/port
    -> IO a                        -- ^ Call if no route found
    -> (Route -> RouteInfo -> IO a)             -- ^ Call if route found
    -> IO a                        -- ^ Result
    
-- Try to route according to preferred host/port; if none is found, 
-- failback to leastconn.
-- We do not count the persistent connections against total connections semaphore,
-- as this is a sum of connections to each agent; number of persistent connections
-- could exceed a maximum for each agent and the semaphore wouldn't behave correctly
withBestRoute rconf uri (Just (prefhost, prefport)) notFound routeFound = do
    withBestRoute rconf uri Nothing notFound routeFound -- Failback
--     findHostRoute rconf uri notFound $ \(HostRoute {hostRoutes, hostSemaphore}) -> do
--         bracket
--             (PQ.getRouteAndPlus1 hostRoutes (prefhost, prefport))
--             (maybe (return ()) PQ.itemMinus1)
--             (\item -> case item of
--                         Nothing -> withBestRoute rconf uri Nothing notFound routeFound -- Failback
--                         Just item -> let route = PQ.valueOf item
--                                      in routeFound route
--                                         `catch` (throwIO . RouteException route) -- Reclass the exception and add the 'Route' parameter
--             )
    
-- Route with least_conn
withBestRoute rconf uri Nothing notFound routeFound = do
    findHostRoute rconf uri notFound $ \(HostRoute {hostRoutes, hostSemaphore}) -> do
        SEM.with hostSemaphore 1 $ 
            bracket
                (PQ.getMinAndPlus1 hostRoutes)
                (maybe (return ()) PQ.itemMinus1)
                (maybe notFound $
                    \item -> let (route, rtinfo) = PQ.valueOf item
                             in routeFound route rtinfo -- MAIN CALL of the code
                                `catch` (throwIO . RouteException route)
                )
