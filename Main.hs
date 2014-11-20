{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


import Network.HTTP.ReverseProxy
import           Network.Wai.Handler.Warp     (defaultSettings, runSettings,
                                               setPort,setNoParsePath)
import qualified Network.HTTP.Client         as HC
import qualified Network.Wai as WAI
import           Network.HTTP.Types           (status404)
import qualified Network.Socket as N
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS
import Control.Exception
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Convertible.Base (convert)
import Data.Convertible.Instances ()
import Data.Int

import RtrNats 
import RouteConfig

routeRequest :: RouteConfig -> WAI.Request -> (WaiProxyResponse -> IO WAI.ResponseReceived) -> IO WAI.ResponseReceived
routeRequest rconf request callProxy = do
        let (Just uri) = BS.takeWhile (/= ':') <$> WAI.requestHeaderHost request
        requestStartTime <- getPOSIXTime
        withBestRoute rconf uri (doRoute requestStartTime) notFound
            `catch` (excConnFailed uri)
    where
        doRoute starttime route@(Route host port _) = 
            let
                proxy = routeToProxy route
                -- TODO - find  where to se application id
                newheaders = ("X-Cf-Applicationid", "")
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
        
        routeToProxy (Route host port (N.SockAddrInet _ hostaddress)) = ProxyDest host port (Just hostaddress)
        routeToProxy (Route host port _) = (ProxyDest host port Nothing)

        -- We need to retype SomeException from RouteException back to normal exception; throw and catch it again
        excConnFailed uri (RouteException route exception) = 
            (throwIO exception) 
            `catch` \e -> case e of
                    (HC.FailedConnectionException2 _ _ _ _) -> failedConn
                    (HC.FailedConnectionException _ _) -> failedConn
                    _ -> throwIO e
            where
                failedConn = do
                    -- Remove hostname from route table
                    routeDel rconf uri route
                    -- Retry with other host
                    routeRequest rconf request callProxy
            

main :: IO ()
main = do
    let url = "nats://127.0.0.1:4222"
    rconf <- newRouteConfig
    startNatsService url rconf
    
--     let settings = HC.defaultManagerSettings  {
--         HC.managerRawConnection = \socket -> HCI.makeConnection (recv socket 131072) (sendAll socket) (sClose socket)
--     }
--     
    HC.withManager HC.defaultManagerSettings $ \manager -> do
        let settings = setPort 2222 $ setNoParsePath True $ defaultSettings
            app = waiProxyToSettings
                    (routeRequest rconf)
                    def{wpsOnExc=(\e -> throw e), 
                        wpsTimeout=Just 30000000,
                        wpsSetIpHeader=SIHFromSocket "X-Forwarded-For"
                    }
                    manager :: WAI.Application
        runSettings settings app
