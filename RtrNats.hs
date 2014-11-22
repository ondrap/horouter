{-# LANGUAGE TemplateHaskell,OverloadedStrings,RecordWildCards,PatternGuards, ScopedTypeVariables #-}
module RtrNats (
    startNatsService
  , registerWebService
  , unregisterWebService
  , getExternalIPs 
) where

import Data.Char (toLower)
import Data.Aeson as AE
import Data.Aeson.TH (deriveJSON, deriveToJSON, defaultOptions, fieldLabelModifier)
import qualified Network.Info as NetInfo
import Data.IP (IPv4, fromHostAddress)
import qualified Network.Socket as NS
import Control.Monad
import Control.Concurrent (forkIO)
import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.ByteString.Char8 as BS
import Control.Exception (bracket, catch, IOException)

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Network.Nats as NATS
import Network.Nats.Json
import Data.Time.Clock (getCurrentTime)

import RouteConfig
import Settings

defaultParallelLimit :: Int
defaultParallelLimit = 20

data RtrStart = RtrStart {
        rtrId :: String
      , rtrHosts :: [IPv4]
      , rtrMinimumRegisterIntervalInSeconds :: Int
    } deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier=(\(c:cs) -> (toLower c):cs) . drop 3} ''RtrStart)

instance ToJSON IPv4 where
    toJSON addr = AE.String $ T.pack $ show addr
    
instance FromJSON IPv4 where
    parseJSON (AE.String str) = return $ read $ T.unpack str
    parseJSON _ = mzero

data RtrRegister = RtrRegister {
        rtrUris :: [BS.ByteString]
      , rtrTags :: Map.Map String T.Text
      , rtrHost ::  BS.ByteString
      , rtrPort :: Int
      , rtrParallelLimit :: Int
      , rtrApp :: BS.ByteString
    } deriving (Show)
    
instance FromJSON BS.ByteString where
    parseJSON (AE.String s) = return $ encodeUtf8 s
    parseJSON _ = mzero
    
instance ToJSON BS.ByteString where
    toJSON str = AE.String $ decodeUtf8 str

$(deriveToJSON defaultOptions{fieldLabelModifier=(\(c:cs) -> (toLower c):cs) . drop 3} ''RtrRegister)
instance FromJSON RtrRegister where
    parseJSON (AE.Object v) =
        RtrRegister <$>
            v .: "uris" <*>
            v .: "tags" .!= Map.empty <*>
            v .: "host" <*>
            v .: "port" <*>
            v .: "parallelLimit" .!= defaultParallelLimit <*>
            v .: "app" .!= ""
    parseJSON _ = mzero
    
getExternalIPs :: IO [IPv4]
getExternalIPs = (map (fromHostAddress . (\(NetInfo.IPv4 x) -> x) . NetInfo.ipv4) . filter isExternal) <$> NetInfo.getNetworkInterfaces
    where
        isExternal (NetInfo.NetworkInterface {name="lo"}) = False
        isExternal (NetInfo.NetworkInterface {ipv4=(NetInfo.IPv4 0)}) = False
        isExternal _ = True
    
localRtrStartMsg :: String -> IO RtrStart
localRtrStartMsg uuid = RtrStart <$> pure uuid <*> getExternalIPs <*> pure defaultStaleInterval
    
answerGreet :: NATS.Nats -> RtrStart -> NATS.MsgCallback
answerGreet nats rtstart _ _ _ (Just reply) = publish nats reply rtstart
answerGreet _ _ _ _ _ _ = return ()

handleRegister :: RouteConfig -> NATS.NatsSID -> String -> RtrRegister -> Maybe String -> IO ()
handleRegister rconf _ _ (RtrRegister {..}) _ = do
    now <- getCurrentTime
    forM_ rtrUris $ \uri -> do
        -- Resolve address
        let hints = NS.defaultHints {
                            NS.addrFlags = [NS.AI_ADDRCONFIG]
                          , NS.addrSocketType = NS.Stream
                        }
        
        addrs <- NS.getAddrInfo (Just hints) (Just $ BS.unpack rtrHost) (Just $ show rtrPort )
            `catch` (\(e :: IOException) -> do
                    putStrLn $ "Registration: " ++ (show $ rtrHost) ++ ": " ++ show e
                    return [])
        -- We support at most 1 address for the endpoint
        if null addrs 
           then return ()
           else let route = Route rtrHost rtrPort (NS.addrAddress $ head addrs) rtrApp now
                in routeAdd rconf uri route rtrParallelLimit

handleUnregister :: RouteConfig -> NATS.NatsSID -> String -> RtrRegister -> Maybe String -> IO ()
handleUnregister rconf _ _ (RtrRegister {..}) _  = 
    forM_ rtrUris $ \uri -> 
        routeDel rconf uri (Route rtrHost rtrPort undefined rtrApp undefined)
        
startNatsService :: RouteConfig -> MainSettings -> IO NATS.Nats
startNatsService rconf settings = do
    rtrstartmsg <- localRtrStartMsg (msetUUID settings)
    nats <- NATS.connectSettings NATS.defaultSettings{
            NATS.natsOnConnect=(onConnect rtrstartmsg)
        }
        
    _ <- NATS.subscribe nats "router.greet" Nothing (answerGreet nats rtrstartmsg)
    _ <- subscribe nats "router.register" Nothing (handleRegister rconf)
    _ <- subscribe nats "router.unregister" Nothing (handleUnregister rconf)
    publish nats "router.start" rtrstartmsg
    
    _ <- forkIO $ purgeStaleRoutes rconf
    return nats
    where
        onConnect msg nats _ = do
            publish nats "router.start" msg
        

registerWebService :: String -> [BS.ByteString] -> BS.ByteString -> Int -> Int -> BS.ByteString -> IO ()
registerWebService natsurl uris host port limit appid = do
    bracket
        (NATS.connect natsurl)
        NATS.disconnect
        (\nats ->
            publish nats "router.register" $ RtrRegister uris Map.empty host port limit appid
        )

unregisterWebService :: String -> [BS.ByteString] -> BS.ByteString -> Int -> BS.ByteString -> IO ()
unregisterWebService natsurl uris host port appid = do
    bracket
        (NATS.connect natsurl)
        NATS.disconnect
        (\nats ->
            publish nats "router.unregister" $ RtrRegister uris Map.empty host port 0 appid
        )

        
