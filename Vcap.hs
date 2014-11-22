{-# LANGUAGE TemplateHaskell,OverloadedStrings,RecordWildCards,PatternGuards, ScopedTypeVariables #-}

module Vcap (
    startVcap,
    makeVcapMsg 
) where
    
import Network.Socket (SockAddr(..))
import Data.Aeson as AE
import Data.Time.Clock
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import Data.Aeson.TH (deriveToJSON, defaultOptions, fieldLabelModifier)
import Data.Char (toLower)
import qualified Data.Text as T
import Data.IP (IPv4, toHostAddress)
import Control.Applicative ((<$>))
import Settings
import Text.Printf (printf)

import Network.Nats (subscribe, Nats)
import Network.Nats.Json (publish)
import RtrNats (getExternalIPs)
    
data VcapAnnounce = VcapAnnounce {
        vcapType :: String
      , vcapIndex :: Int
      , vcapHost :: SockAddr
      , vcapCredentials :: (String, String)
      , vcapStart :: String
      , vcapUptime :: String
      , vcapUuid :: String
    } deriving (Show)
$(deriveToJSON defaultOptions{fieldLabelModifier=(map toLower . drop 4)} ''VcapAnnounce)

instance ToJSON SockAddr where
    toJSON addr = AE.String $ T.pack $ (show addr)

startVcap :: Nats -> MainSettings -> IO ()
startVcap nats settings = do
    iface <- head <$> getExternalIPs
    now <- getCurrentTime
    let msg = makeVcapMsg settings iface now
    _ <- subscribe nats "vcap.component.discover" Nothing (handleDiscover iface)
    publish nats "vcap.component.announce" msg
    return ()
    where 
        handleDiscover iface _ _ _ (Just reply) = do
            now <- getCurrentTime
            let msg = makeVcapMsg settings iface now 
            publish nats reply msg
        handleDiscover _ _ _ _ Nothing      = return ()
    
makeVcapMsg :: MainSettings -> IPv4 -> UTCTime -> VcapAnnounce
makeVcapMsg settings iface now = 
    let
        iuptime = (round $ toRational $ (now `diffUTCTime` (msetStart settings))) :: Int
        days = iuptime `div` 86400
        hours = (iuptime `mod` 86400) `div` 3600
        minutes = (iuptime `mod` 3600) `div` 60
        seconds = iuptime `mod` 60
        uptime = printf "%dd:%dh:%dm:%ds" days hours minutes seconds
    in
        VcapAnnounce{
            vcapType="Router",
            vcapIndex=msetIndex settings,
            vcapHost=SockAddrInet (fromIntegral $ msetPort settings) (toHostAddress iface),
            vcapCredentials=(msetUser settings, msetPassword settings),
            vcapUuid=msetUUID settings,
            vcapStart=formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" (msetStart settings),
            vcapUptime=uptime
        }
