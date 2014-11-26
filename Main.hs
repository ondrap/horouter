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
import qualified Network.HTTP.Types as HT
import Web.Cookie (parseCookies, parseSetCookie, renderSetCookie, SetCookie(..))
import Control.Exception
import Data.Time.Clock (getCurrentTime)
import Data.Convertible.Base (convert)
import Data.Convertible.Instances ()
import Control.Concurrent (threadDelay)
import Data.Int
import Data.UUID.V4
import Settings
import System.Timeout
import Blaze.ByteString.Builder as BLDR

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

-- | Delay between router.start NATS message and actual start of routing
startProxyDelay :: Int
startProxyDelay = 3000000

vcapCookieName :: BS.ByteString
vcapCookieName = "__VCAP_ID__"

stickyCookieName :: BS.ByteString
stickyCookieName = "JSESSIONID"

defProxySettings :: WaiProxySettings
defProxySettings = def{
                    wpsOnExc=(\e -> throw e), 
                    wpsTimeout=Just httpRequestTimeout,
                    wpsSetIpHeader=SIHFromSocket "X-Forwarded-For"
                }

routeRequest :: RouteConfig -> HC.Manager -> WAI.Application
routeRequest rconf manager req sendResponse = do
    requestStartTime <- getCurrentTime
    let (Just uri) = BS.takeWhile (/= ':') <$> WAI.requestHeaderHost req
        prefhost = getPrefHost req
    
    (withBestRoute rconf uri prefhost notFound $ 
        \(route@(Route {routeHost=host, routePort=port}), rtinfo@(RouteInfo {routeAppId=appid})) -> do
            let
                haddr = routeToHostAddr rtinfo
                newheaders = ("X-Cf-Applicationid", appid)
                        : ("X-Cf-Instanceid", BS.concat [host, ":", (BS.pack $ show port)])
                        : ("X-Request-Start", BS.pack $ show (truncate (1000 * (convert requestStartTime :: Rational)) :: Int64))
                        : WAI.requestHeaders req
                newreq = req{WAI.requestHeaders=newheaders}
                proxySettings = defProxySettings {
                    wpsProcessHeaders = (addVcapId route)
                }
                
            proxyStartTime <- getCurrentTime
            result <- try (waiDoProxy proxySettings manager host port haddr newreq sendResponse)
            proxyEndTime <- getCurrentTime
            case result of
                Right res -> do
                    -- TODO logging
                    return res
                Left (e :: SomeException) -> do
                    -- TODO logging
                    throwIO e
        ) `catch` (excConnFailed uri)
    where
        -- What happens when there is no mapping
        notFound = sendResponse $ WAI.responseLBS status404 [] "Route to host not found."
        
        routeToHostAddr (RouteInfo {routeSockAddr=(N.SockAddrInet _ hostaddress)}) = Just hostaddress
        routeToHostAddr _ = Nothing
        
        -- Add cookie __VCAP_ID__ to headers if session cookie is present
        addVcapId :: Route -> HT.RequestHeaders -> HT.RequestHeaders
        addVcapId (Route host port) headers = 
            if hasSticky
                then (("Set-Cookie", render cookie):headers)
                else headers 
            where
                render = BLDR.toByteString . renderSetCookie
                hasSticky = not $ null stickyCookies
                setcookies = map (parseSetCookie . snd) $ filter (\hdr -> fst hdr == "set-cookie") headers
                stickyCookies = filter ((== stickyCookieName) . setCookieName) setcookies
                cookie = def {
                    setCookieName = vcapCookieName,
                    setCookieValue = BS.concat [host, BS.pack (':':(show port))],
                    setCookieHttpOnly = True,
                    setCookiePath = Just "/"
                }
            
                                               
        -- Try to decide for a preferred host if both stickyCookieName and vcapCookieName exist
        getPrefHost req' = do -- Maybe Monad
            cookies <- parseCookies <$> lookup "cookie" (WAI.requestHeaders req')
            _ <- lookup stickyCookieName cookies -- Exit function if this fails
            prefcookie <- lookup vcapCookieName cookies
            let (host, sport) = BS.span (/=':') prefcookie
            case (reads $ drop 1 $ BS.unpack sport) of
                    ((port, _):_) -> Just (Route host port)
                    _             -> Nothing

        -- This happens when an exception happens
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
                            sendResponse $ WAI.responseLBS status502 [] "Error when contacting app agent."
                         _ -> do
                            -- Remove hostname from route table, if this is not the last route
                            routeDel rconf uri route
                            -- Retry with other host
                            routeRequest rconf manager req sendResponse

                            
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
    threadDelay startProxyDelay
    putStrLn "Serving requests."
    
    -- Use 128K blocks for data transfer using HTTP, make the initial connection timeout faster
    let managerSettings = HC.defaultManagerSettings  {
        HC.managerRawConnection = return $ openSocketConnectionTimeoutSize clientConnectTimeout (const $ return ()) httpChunkSize
    }
    
    let webSettings = setPort (msetPort settings) $ setNoParsePath True $ defaultSettings
    
    HC.withManager managerSettings $ \manager -> do
        let app = routeRequest rconf manager
        runSettings webSettings app
