{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, DeriveDataTypeable #-}

module RouteConfig (
    RouteConfig
  , Route(..)
  , newRouteConfig
  , routeAdd
  , routeDel
  , withBestRoute 
  , RouteException(..)
) where
    
import qualified PrioQueueHeap as PQ
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import qualified Data.Map.Strict as M
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS
import Control.Exception (bracket, catch, throwIO, SomeException, Exception(..))
import qualified Data.CaseInsensitive as CI
import qualified Network.Socket as N
import Data.Typeable
import qualified Control.Concurrent.MSemN as SEM


data Route = Route BS.ByteString Int N.SockAddr
-- Allow N.SockAddr to be undefined for some operations
instance Show Route where
    show (Route h1 p1 _) = (BS.unpack h1) ++ ":" ++ (show p1)

-- Ignore hostaddress when comparing
instance Eq Route where
    (Route h1 p1 _) == (Route h2 p2 _) = (h1, p1) == (h2, p2)
instance Ord Route where
    (Route h1 p1 _) `compare` (Route h2 p2 _) = (h1, p1) `compare` (h2, p2)

data HostRoute = HostRoute {
        hostRoutes :: PQ.Queue Route,
        hostSemaphore :: SEM.MSemN Int
    }
    
data RouteConfig = RouteConfig {
        routeMap :: MVar (M.Map (CI.CI BS.ByteString) HostRoute)
    }

-- TODO: pruning po 2 minutach
    
newRouteConfig :: IO RouteConfig
newRouteConfig = RouteConfig <$> newMVar M.empty

routeAdd :: RouteConfig 
    -> BS.ByteString  -- ^ Domain mapping
    -> Route          -- ^ IP address:port of instance
    -> Int            -- ^ Maximum parallel operations on this route
    -> IO ()
routeAdd _ _ _ limit
    | limit <= 0 = putStrLn "Incorrect parallelism limit"
routeAdd (RouteConfig {routeMap}) uri addr limit = modifyMVar_ routeMap $ \rmap -> do
    putStrLn $ "Registering web service: " ++ (show uri) ++ " -> " ++ (show addr) ++ " parallel limit: " ++ (show limit)
    case (M.lookup iuri rmap) of
        Just hroute -> do
            let hq = hostRoutes hroute
                hsem = hostSemaphore hroute
            hasroute <- PQ.lookup addr hq
            case hasroute of
                    Nothing -> do
                        SEM.signal hsem limit
                        _ <- PQ.insert (0, addr) limit hq
                        return ()
                    Just (priority, limit)-> do
                        -- Update limit of an item if it is different
                        oldlimit <- PQ.adjustLimit addr limit hq
                        case () of
                           _| oldlimit == 0    -> return () -- not found, shouldn't happen as we are locked in MVAR?
                            | limit > oldlimit -> SEM.signal hsem (limit - oldlimit)
                            | limit < oldlimit -> (forkIO $ SEM.wait hsem (oldlimit - limit)) >> return ()
                            | otherwise        -> return ()
            return rmap
        Nothing -> do
            q <- PQ.newQueue
            _ <- PQ.insert (0, addr) limit q
            msem <- SEM.new limit
            return $ M.insert iuri (HostRoute q msem) rmap
    where
        iuri = CI.mk uri

routeDel :: RouteConfig 
    -> BS.ByteString    -- ^ Domain mapping
    -> Route            -- ^ IP address:port of instance
    -> IO ()
routeDel (RouteConfig {routeMap}) uri addr = modifyMVar_ routeMap $ \rmap -> do
    putStrLn $ "Unregistering web service: " ++ (show uri) ++ " -> " ++ (show addr)
    case (M.lookup iuri rmap) of
        Nothing -> return rmap
        Just hroute -> do
            item <- PQ.lookup addr (hostRoutes hroute)
            case item of
                 Just (_, limit) -> do
                    PQ.delete addr (hostRoutes hroute)
                    forkIO $ SEM.wait (hostSemaphore hroute) limit
                    return ()
                 Nothing ->
                    return ()

            size <- PQ.size $ hostRoutes hroute
            case size of
                0 -> do
                    -- Add something to the semaphore, so that things on the semaphore will
                    -- eventually get through
                    SEM.signal (hostSemaphore hroute) 1
                    return $ M.delete iuri rmap
                _ -> do
                    return rmap
    where
        iuri = CI.mk uri

data RouteException = RouteException Route SomeException
    deriving (Show, Typeable)
instance Exception RouteException
        
-- | Run some code while ensuring correct counting of connections in queue
withBestRoute :: RouteConfig 
    -> BS.ByteString 
    -> (Route -> IO a)
    -> IO a
    -> IO a
withBestRoute (RouteConfig {routeMap}) uri routeFound notFound = do
    rmap <- readMVar routeMap
    case (M.lookup (CI.mk uri) rmap) of
        Nothing -> notFound
        Just (HostRoute {hostRoutes, hostSemaphore}) -> do
            SEM.with hostSemaphore 1 $ 
                bracket
                    (PQ.getMinAndPlus1 hostRoutes)
                    (maybe (return ()) PQ.itemMinus1)
                    (maybe notFound $
                        \item -> let route = PQ.valueOf item
                                in routeFound route
                                    `catch` (throwIO . RouteException route)
                    )