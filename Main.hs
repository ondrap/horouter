{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NamedFieldPuns #-}


import Network.HTTP.ReverseProxy
import           Network.Wai.Handler.Warp     (defaultSettings, runSettings,
                                               setPort,setNoParsePath)
import qualified Network.HTTP.Client         as HC
import Network.HTTP.Client.Internal (openSocketConnectionSize, Connection)
import qualified Network.Wai as WAI
import           Network.HTTP.Types           (status404, status502)
import qualified Network.Socket as N
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS
import Control.Exception
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock
import Data.Convertible.Base (convert)
import Data.Convertible.Instances ()
import Control.Concurrent (threadDelay)
import Data.Int
import Data.UUID.V4
import Settings
import System.Timeout

import RtrNats 
import RouteConfig
import Vcap


-- | Timeout for TCP connect to the client
clientConnectTimeout :: Int
clientConnectTimeout = 2 * 1000000

-- | Timeout for the HTTP request to start sending data
httpRequestTimeout :: Int
httpRequestTimeout = 30 * 1000000

-- | Block size that should be used to transfer data between APP instance and client
httpChunkSize :: Int
httpChunkSize = 128 * 1024

proxySettings = def{wpsOnExc=(\e -> throw e), 
                    wpsTimeout=Just httpRequestTimeout,
                    wpsSetIpHeader=SIHFromSocket "X-Forwarded-For"
                }

routeRequest :: RouteConfig -> WAI.Request -> (WaiProxyResponse -> IO WAI.ResponseReceived) -> IO WAI.ResponseReceived
routeRequest rconf request callProxy = do
        let (Just uri) = BS.takeWhile (/= ':') <$> WAI.requestHeaderHost request
        requestStartTime <- getPOSIXTime
        withBestRoute rconf uri (doRoute requestStartTime) notFound
            `catch` (excConnFailed uri)
    where
        doRoute starttime route@(Route {routeHost=host, routePort=port, routeAppId=appid}) = 
            let
                proxyDest = routeToProxyDest route
                newheaders = ("X-Cf-Applicationid", appid)
                           : ("X-Cf-Instanceid", BS.concat [host, ":", (BS.pack $ show port)])
                           : ("X-Request-Start", BS.pack $ show (truncate (1000 * (convert starttime :: Rational)) :: Int64))
                           : WAI.requestHeaders request
                newreq = request{WAI.requestHeaders=newheaders}
            in do
                proxyStartTime <- getPOSIXTime
                result <- try (callProxy $ WPRModifiedRequest newreq proxy proxySettings)
                proxyEndTime <- getPOSIXTime
                case result of
                    Right res -> do
                        -- TODO logging
                        return res
                    Left (e :: SomeException) -> do
                        -- TODO logging
                        throwIO e
                
        notFound = callProxy $ WPRResponse (WAI.responseLBS status404 [] "Route to host not found.")
        
        routeToProxyDest (Route {routeHost,routePort,routeSockAddr=(N.SockAddrInet _ hostaddress)}) = 
                ProxyDest routeHost routePort (Just hostaddress)
        routeToProxyDest (Route {routeHost, routePort}) = 
                ProxyDest routeHost routePort Nothing

        -- We need to retype SomeException from RouteException back to normal exception; throw and catch it again
        excConnFailed uri (RouteException route exception) = 
            (throwIO exception) 
            `catch` \e -> case e of
                    (HC.FailedConnectionException2 _ _ _ _) -> failedConn
                    (HC.FailedConnectionException _ _)      -> failedConn
                    _ -> throwIO e
            where
                failedConn = do
                    -- If this is the last route, don't remove it from the route table
                    numroutes <- routeSize rconf uri
                    case numroutes of
                         1 -> 
                            callProxy $ WPRResponse (WAI.responseLBS status502 [] "Error when contacting app agent.")
                         _ -> do
                            -- Remove hostname from route table, if this is not the last route
                            routeDel rconf uri route
                            -- Retry with other host
                            routeRequest rconf request callProxy

                            
-- | Wrapper for opening connection to support different timeout on socket connect
openSocketConnectionTimeoutSize :: Int -> (N.Socket -> IO ()) -> Int
                         -> Maybe N.HostAddress -> String -> Int
                         -> IO Connection
openSocketConnectionTimeoutSize wtimeout a b c host port = do
    res <- timeout wtimeout $ openSocketConnectionSize a b c host port
    case res of
         Just x -> return x
         Nothing -> throwIO $ HC.FailedConnectionException host port
                    
main :: IO ()
main = do
    -- Build settings
    uuid <- nextRandom
    now <- getCurrentTime
    let rtindex = 0
    let settings = defaultMainSettings{
            msetUUID = (show rtindex) ++ "-" ++ (show uuid)
          , msetIndex = rtindex
          , msetStart = now
        }
        
    -- Start background services
    rconf <- newRouteConfig
    nats <- startNatsService rconf settings
    startVcap nats settings
    
    -- Wait a moment until we get some messages over NATS
    putStrLn "Waiting for instances to publish their mappings..."
    threadDelay (msetStartDelay settings)
    putStrLn "Serving requests."
    
    -- Use 128K blocks for data transfer using HTTP, make the initial connection timeout faster
    let managerSettings = HC.defaultManagerSettings  {
        HC.managerRawConnection = return $ openSocketConnectionTimeoutSize clientConnectTimeout (const $ return ()) httpChunkSize
    }
    
    let webSettings = setPort (msetPort settings) $ setNoParsePath True $ defaultSettings
    
    HC.withManager managerSettings $ \manager -> do
        let app = waiProxyToSettings (routeRequest rconf) proxySettings manager
        runSettings webSettings app
