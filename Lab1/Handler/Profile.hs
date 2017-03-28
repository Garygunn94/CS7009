




{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Handler.Profile where

import Import hiding (unpack, pack)
import Data.List hiding(intercalate, map, lookup)
import Data.Maybe
import Data.ByteString.Char8 as DBC hiding (putStrLn)
import Data.Bson.Generic
import Data.Aeson
import Data.Text.Encoding
import Data.Vector as V hiding (mapM, map, mapM_)
import Data.Text as DT hiding(intercalate, map) 
import CommonResources
import Control.Concurrent
import              Servant
import              Servant.API
import              Servant.Client
import              Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Data.Map as M hiding (split)
import Database.MongoDB
import MongodbHelpers
import System.IO
import Data.List as DL
import Prelude (read)

crawlerApi :: Servant.Proxy CrawlerApi
crawlerApi = Servant.Proxy

starter :: CommonResources.User -> ClientM CommonResources.Response
end :: CommonResources.User -> ClientM CommonResources.Response

starter :<|> end = client crawlerApi

getProfileR :: Import.Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
    	sess <- getSession
    	let access_token = DBC.unpack (fromJust $ Import.lookup "access_token" sess)
    	let uname = DBC.unpack (fromJust $ Import.lookup "login" sess)
        let userdata = CommonResources.User (uname) (access_token) 3
        liftIO $ makeApiCall userdata
        setTitle . toHtml $ (DT.pack uname) <> "'s User page"
        $(widgetFile "profile")


makeApiCall :: CommonResources.User -> IO ()
makeApiCall user = liftIO $ do
  manager <- Network.HTTP.Client.newManager Network.HTTP.Client.defaultManagerSettings
  res <- runClientM (starter user) (ClientEnv manager (BaseUrl Http crawlerhost (read(crawlerport) :: Int) ""))
  case res of
    Left err -> Import.putStrLn $ "Error: "
    Right response -> return ()




