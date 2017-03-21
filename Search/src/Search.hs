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
{-# LANGUAGE RecordWildCards #-} 

module Search where
import           Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Network.Wai hiding(Response)
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import Control.Concurrent
import           Servant
import           Servant.API
import           Servant.Client
import           System.IO
import qualified Data.List as DL hiding (find)
import Data.Maybe (catMaybes, fromJust)
import Control.Monad (when, liftM)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import CommonResources
import MongodbHelpers
import Database.MongoDB
import Network.Wai.Middleware.Cors
import Neo4jHelpers
import Servant.JS

type ApiHandler = ExceptT ServantErr IO

searchApi :: Proxy SearchApi
searchApi = Proxy

type SearchApi' = SearchApi
       :<|> Raw

searchApi' :: Proxy SearchApi'
searchApi' = Proxy

www :: FilePath
www = "www"

server :: Server SearchApi
server = Search.getSocialGraph

server' :: Server SearchApi'
server' = server
   :<|> serveDirectory www -- serve static files

searchApp :: Application
searchApp = simpleCors (serve searchApi' server')

mkApp :: IO()
mkApp = do
    putStrLn ("Search starting on port: " ++ searchport)
    writeJSForAPI searchApi jquery (www ++ "/" ++ "jquery" ++ "/" ++ "api.js")
    run (read (searchport) ::Int) searchApp 


getSocialGraph :: ApiHandler SocialGraph
getSocialGraph = liftIO $ do
	putStrLn ("Fetching Node data")
	sg <- Neo4jHelpers.getSocialGraph
        return sg
	{--nodes <- MongodbHelpers.withMongoDbConnection $ do
		docs <- find (select [] "Node_RECORD") >>= drainCursor
		return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Node) docs
        putStrLn ("Fetching Link Data")
        links <- MongodbHelpers.withMongoDbConnection $ do
		docs <- find (select [] "Link_RECORD") >>= drainCursor
		return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CommonResources.Link) docs
        let socialGraph = (SocialGraph nodes links)
        return socialGraph-}

