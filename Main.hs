{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NamedFieldPuns #-}


import Network.HTTP.ReverseProxy
import           Network.Wai.Handler.Warp     (defaultSettings, runSettings,
                                               setPort,setNoParsePath)
import qualified Network.HTTP.Client         as HC
import Network.HTTP.Client.Internal (openSocketConnection)
import qualified Network.Wai as WAI
import           Network.HTTP.Types           (status404)
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
import Data.UUID (UUID)
import Data.UUID.V4
import Settings

import RtrNats 
import RouteConfig
import Vcap

routeRequest :: RouteConfig -> WAI.Request -> (WaiProxyResponse -> IO WAI.ResponseReceived) -> IO WAI.ResponseReceived
routeRequest rconf request callProxy = do
        let (Just uri) = BS.takeWhile (/= ':') <$> WAI.requestHeaderHost request
        requestStartTime <- getPOSIXTime
        withBestRoute rconf uri (doRoute requestStartTime) notFound
            `catch` (excConnFailed uri)
    where
        doRoute starttime route@(Route {routeHost=host, routePort=port, routeAppId=appid}) = 
            let
                proxy = routeToProxy route
                newheaders = ("X-Cf-Applicationid", appid)
                           : ("X-Cf-Instanceid", BS.concat [host, ":", (BS.pack $ show port)])
                           : ("X-Request-Start", BS.pack $ show (truncate (1000 * (convert starttime :: Rational)) :: Int64))
                           : WAI.requestHeaders request
                newreq = request{WAI.requestHeaders=newheaders}
            in do
                proxyStartTime <- getPOSIXTime
                result <- try (callProxy $ WPRModifiedRequest newreq proxy)
                proxyEndTime <- getPOSIXTime
                case result of
                    Right res -> do
                        -- TODO logging
                        return res
                    Left (e :: SomeException) -> do
                        -- TODO logging
                        throwIO e
                
        notFound = callProxy $ WPRResponse (WAI.responseLBS status404 [] "Route to host not found.")
        
        routeToProxy (Route {routeHost,routePort,routeSockAddr=(N.SockAddrInet _ hostaddress)}) = 
                ProxyDest routeHost routePort (Just hostaddress)
        routeToProxy (Route {routeHost, routePort}) = 
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
                    -- Remove hostname from route table
                    routeDel rconf uri route
                    -- Retry with other host
                    routeRequest rconf request callProxy

main :: IO ()
main = do
    uuid <- nextRandom
    now <- getCurrentTime
    let rtindex = 0
    let settings = MainSettings {
            msetIndex = rtindex
          , msetPort = 2222
          , msetUser = ""
          , msetPassword = ""
          , msetUUID = (show rtindex) ++ "-" ++ (show uuid)
          , msetStart = now
        }
        
    rconf <- newRouteConfig
    nats <- startNatsService rconf settings
    startVcap nats settings
    
    putStrLn "Waiting to serve requests..."
    -- Wait a moment until we get some messages over NATS
    threadDelay 2000000
    putStrLn "Serving requests."
    
    -- Use 128K blocks for data transfer using HTTP
    let managerSettings = HC.defaultManagerSettings  {
        HC.managerRawConnection = return $ openSocketConnection (const $ return ()) 131072
    }
    
    let webSettings = setPort (msetPort settings) $ setNoParsePath True $ defaultSettings
    
    HC.withManager managerSettings $ \manager -> do
        let app = waiProxyToSettings
                (routeRequest rconf)
                def{wpsOnExc=(\e -> throw e), 
                    wpsTimeout=Just 30000000,
                    wpsSetIpHeader=SIHFromSocket "X-Forwarded-For"
                }
                manager :: WAI.Application
        runSettings webSettings app
