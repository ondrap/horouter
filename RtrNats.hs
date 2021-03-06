{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module RtrNats (
    startNatsService
  , registerWebService
  , unregisterWebService
  , getExternalIPs
) where

import           Control.Applicative   (pure, (<$>), (<*>))
import           Control.Concurrent    (forkIO)
import           Control.Exception     (IOException, bracket, catch)
import           Control.Monad
import           Data.Aeson            as AE
import           Data.Aeson.TH         (defaultOptions, deriveJSON,
                                        deriveToJSON, fieldLabelModifier)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (toLower)
import           Data.IP               (IPv4, fromHostAddress)
import qualified Network.Info          as NetInfo
import qualified Network.Socket        as NS

import qualified Data.Map              as Map
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import           Data.Time.Clock       (addUTCTime, getCurrentTime)
import qualified Network.Nats          as NATS
import           Network.Nats.Json

import           RouteConfig
import           Settings
import           Utils

defaultParallelLimit :: Int
defaultParallelLimit = 20

data RtrStart = RtrStart {
        rtrId                               :: String
      , rtrHosts                            :: [IPv4]
      , rtrMinimumRegisterIntervalInSeconds :: Int
    } deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier=(\(c:cs) -> toLower c : cs) . drop 3} ''RtrStart)

instance ToJSON IPv4 where
    toJSON addr = AE.String $ T.pack $ show addr

instance FromJSON IPv4 where
    parseJSON (AE.String str) = return $ read $ T.unpack str
    parseJSON _ = mzero

data RtrRegister = RtrRegister {
        rtrUris                    :: [BS.ByteString]
      , rtrTags                    :: Map.Map String T.Text
      , rtrHost                    ::  BS.ByteString
      , rtrPort                    :: Int
      , rtrParallelLimit           :: Int
      , rtrApp                     :: BS.ByteString
      , rtrPrivateInstanceId       :: BS.ByteString
      , rtrStaleThresholdInSeconds :: Maybe Int
    } deriving (Show)

instance FromJSON BS.ByteString where
    parseJSON (AE.String s) = return $ encodeUtf8 s
    parseJSON _ = mzero

instance ToJSON BS.ByteString where
    toJSON str = AE.String $ decodeUtf8 str

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo_ . drop 3} ''RtrRegister)
instance FromJSON RtrRegister where
    parseJSON (AE.Object v) =
        RtrRegister <$>
            v .: "uris" <*>
            v .:? "tags" .!= Map.empty <*>
            v .: "host" <*>
            v .: "port" <*>
            v .:? "parallel_limit" .!= defaultParallelLimit <*>
            v .:? "app" .!= "" <*>
            v .:? "private_instance_id" .!= "" <*>
            v .:? "stale_threshold_in_seconds"
    parseJSON _ = mzero

getExternalIPs :: IO [IPv4]
getExternalIPs = (map (fromHostAddress . (\(NetInfo.IPv4 x) -> x) . NetInfo.ipv4) . filter isExternal) <$> NetInfo.getNetworkInterfaces
    where
        isExternal (NetInfo.NetworkInterface {name="lo"}) = False
        isExternal (NetInfo.NetworkInterface {name="lo0"}) = False
        isExternal (NetInfo.NetworkInterface {ipv4=(NetInfo.IPv4 0)}) = False
        isExternal _ = True


-- | TODO -
localRtrStartMsg :: String -> IO RtrStart
localRtrStartMsg uuid = RtrStart <$> pure uuid <*> getExternalIPs <*> pure defaultStaleInterval

answerGreet :: NATS.Nats -> RtrStart -> NATS.MsgCallback
answerGreet nats rtstart _ _ _ (Just reply) = publish nats reply rtstart
answerGreet _ _ _ _ _ _ = return ()

handleRegister :: RouteConfig -> MainSettings -> NATS.NatsSID -> String -> RtrRegister -> Maybe String -> IO ()
handleRegister rconf settings _ _ (RtrRegister {..}) _ = do
    now <- getCurrentTime
    let defaultStale = fromIntegral $ msetDropletStaleThreshold settings
        expireTime = case () of
            _ | (Just stale) <- rtrStaleThresholdInSeconds, fromIntegral stale < defaultStale
                     -> fromIntegral stale `addUTCTime` now
              | otherwise -> defaultStale `addUTCTime` now
    forM_ rtrUris $ \uri -> do
        -- Resolve address
        let hints = NS.defaultHints {
                            NS.addrFlags = [NS.AI_ADDRCONFIG]
                          , NS.addrSocketType = NS.Stream
                        }

        addrs <- NS.getAddrInfo (Just hints) (Just $ BS.unpack rtrHost) (Just $ show rtrPort )
            `catch` (\(e :: IOException) -> do
                    putStrLn $ "Registration: " ++ show rtrHost ++ ": " ++ show e
                    return [])
        -- We support at most 1 address for the endpoint
        unless (null addrs) $
                let route = Route rtrHost rtrPort
                    rtinfo = RouteInfo (NS.addrAddress $ head addrs) rtrApp expireTime
                in routeAdd rconf uri route rtinfo rtrParallelLimit

handleUnregister :: RouteConfig -> NATS.NatsSID -> String -> RtrRegister -> Maybe String -> IO ()
handleUnregister rconf _ _ (RtrRegister {..}) _  =
    forM_ rtrUris $ \uri ->
        routeDel rconf uri (Route rtrHost rtrPort)

startNatsService :: RouteConfig -> MainSettings -> IO NATS.Nats
startNatsService rconf settings = do
    rtrstartmsg <- localRtrStartMsg (msetUUID settings)
    nats <- NATS.connectSettings NATS.defaultSettings{
            NATS.natsHosts = msetNats settings,
            NATS.natsOnReconnect = onReconnect rtrstartmsg
        }

    _ <- NATS.subscribe nats "router.greet" Nothing (answerGreet nats rtrstartmsg)
    _ <- subscribe nats "router.register" Nothing (handleRegister rconf settings)
    _ <- subscribe nats "router.unregister" Nothing (handleUnregister rconf)
    publish nats "router.start" rtrstartmsg

    _ <- forkIO $ pruneStaleRoutes settings rconf
    return nats
    where
        onReconnect msg nats _ =
            publish nats "router.start" msg


registerWebService :: String -> [BS.ByteString] -> BS.ByteString -> Int -> Int -> BS.ByteString -> BS.ByteString -> IO ()
registerWebService natsurl uris host port limit appid instid =
    bracket
        (NATS.connect natsurl)
        NATS.disconnect
        (\nats ->
            publish nats "router.register" $ RtrRegister uris Map.empty host port limit appid instid Nothing
        )

unregisterWebService :: String -> [BS.ByteString] -> BS.ByteString -> Int -> BS.ByteString -> BS.ByteString -> IO ()
unregisterWebService natsurl uris host port appid instid =
    bracket
        (NATS.connect natsurl)
        NATS.disconnect
        (\nats ->
            publish nats "router.unregister" $ RtrRegister uris Map.empty host port 0 appid instid Nothing
        )
