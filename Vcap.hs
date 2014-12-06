{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Vcap (
    startVcap,
    makeVcapMsg
) where

import           Data.Aeson          as AE
import           Data.Time.Clock
import           Data.Time.Format    (formatTime)
import           Network.Socket      (SockAddr (..))
import           System.Locale       (defaultTimeLocale)

import           Control.Applicative ((<$>))
import           Data.Aeson.TH       (defaultOptions, deriveToJSON,
                                      fieldLabelModifier)
import           Data.Char           (toLower)
import           Data.IP             (IPv4, toHostAddress)
import qualified Data.Text           as T
import           Settings            (MainSettings (..))
import           Text.Printf         (printf)

import           Network.Nats        (Nats, subscribe)
import           Network.Nats.Json   (publish)
import           RtrNats             (getExternalIPs)

data VcapAnnounce = VcapAnnounce {
        vcapType        :: String
      , vcapIndex       :: Int
      , vcapHost        :: SockAddr
      , vcapCredentials :: (String, String)
      , vcapStart       :: String
      , vcapUptime      :: String
      , vcapUuid        :: String
    } deriving (Show)
$(deriveToJSON defaultOptions{fieldLabelModifier = map toLower . drop 4} ''VcapAnnounce)

instance ToJSON SockAddr where
    toJSON addr = AE.String $ T.pack (show addr)

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
        iuptime = (round $ toRational (now `diffUTCTime` msetStart settings)) :: Int
        days = iuptime `div` 86400
        hours = (iuptime `mod` 86400) `div` 3600
        minutes = (iuptime `mod` 3600) `div` 60
        seconds = iuptime `mod` 60
        uptime = printf "%dd:%dh:%dm:%ds" days hours minutes seconds
    in
        -- TODO - publikovat credentials na stavovy web server
        VcapAnnounce{
            vcapType="Router",
            vcapIndex=msetIndex settings,
            vcapHost=SockAddrInet (fromIntegral $ msetPort settings) (toHostAddress iface),
            vcapCredentials=("", ""),
            vcapUuid=msetUUID settings,
            vcapStart=formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" (msetStart settings),
            vcapUptime=uptime
        }
