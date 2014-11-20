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
    
import qualified PrioQueue as PQ
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


data Route = Route {
        routeHost :: BS.ByteString 
      , routePort :: Int 
      , routeSockAddr :: N.SockAddr 
      , routeAppId :: BS.ByteString
    }
-- Allow N.SockAddr to be undefined for some operations
instance Show Route where
    show (Route {routeHost, routePort}) = (BS.unpack routeHost) ++ ":" ++ (show routePort)

-- Ignore hostaddress when comparing
instance Eq Route where
    (Route {routeHost=h1,routePort=p1}) == (Route {routeHost=h2,routePort=p2}) = (h1, p1) == (h2, p2)
instance Ord Route where
    (Route {routeHost=h1,routePort=p1}) `compare` (Route {routeHost=h2,routePort=p2}) = (h1, p1) `compare` (h2, p2)

data HostRoute = HostRoute {
        hostRoutes :: PQ.Queue Route
      , hostSemaphore :: SEM.MSemN Int
    }
    
data RouteConfig = RouteConfig {
        routeMap :: MVar (M.Map (CI.CI BS.ByteString) HostRoute)
    }

-- TODO: pruning po 2 minutach
    
newRouteConfig :: IO RouteConfig
newRouteConfig = RouteConfig <$> newMVar M.empty

routeAdd :: RouteConfig 
    -> BS.ByteString  -- ^ Domain mapping
    -> Route          -- ^ IP address:port of instance + some info
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
            moldlimit <- PQ.adjustLimit addr limit hq
            case moldlimit of
                    Nothing -> do
                        SEM.signal hsem limit
                        PQ.insert (0, addr) limit hq
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
            PQ.insert (0, addr) limit q
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
                    _ <- forkIO $ SEM.wait (hostSemaphore hroute) limit
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
                                 in routeFound route -- MAIN CALL of the code
                                    `catch` (throwIO . RouteException route)
                    )
