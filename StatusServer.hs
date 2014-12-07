{-# LANGUAGE OverloadedStrings   #-}

module StatusServer (
  serveStatusServer
) where

import qualified Network.Wai                  as WAI
import           Network.Wai.Handler.Warp     (defaultSettings, runSettings,
                                               setPort)
import Network.Wai.Middleware.HttpAuth
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types (status200, status404)
import qualified Data.Map as M
import qualified Data.Aeson as AE

import Settings
import RouteConfig

statusServer :: RouteConfig -> WAI.Application
statusServer rconf req respond = handle (WAI.pathInfo req)
  where
    handle ["routes"] = do
      routes <- dumpRtConf rconf
      let jsroutes = M.mapKeys BS.unpack $ fmap (map show) routes
      respond $ WAI.responseLBS status200 [("Content-Type", "application/json")] $ AE.encode jsroutes
    handle ["healthz"] = respond $ WAI.responseLBS status200 [("Content-Type", "text/plain")] "ok"
    handle _ = respond $ WAI.responseLBS status404 [("Content-Type", "text/plain")] "Resource not found"


serveStatusServer :: RouteConfig -> MainSettings -> IO ()
serveStatusServer rconf settings = do
  let webSettings = setPort (stPort $ msetStatusSettings settings) defaultSettings
  let app = basicAuth checkCred "StatusServer" (statusServer rconf)
  runSettings webSettings app
  where
    checkCred user pass = return $ (BS.unpack user, BS.unpack pass) == stCred (msetStatusSettings settings)
