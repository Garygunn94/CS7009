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
import qualified Data.Text as DT
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
server = Search.getSocialGraph :<|>
         Search.getLanguageChart :<|> 
         Search.getRepoSizeChart :<|> 
         Search.getUserBubbleChart :<|>
         Search.getLocationBubbleChart :<|>
         Search.getCompanyBubbleChart


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


getLanguageChart :: ApiHandler LanguageChart
getLanguageChart = liftIO $ do
	putStrLn "Fetching Language Chart"
	lc <- Neo4jHelpers.getLanguageChart
	return lc

getRepoSizeChart :: ApiHandler RepoSizeChart
getRepoSizeChart = liftIO $ do
	putStrLn "Fetching RepoSize Chart"
	lc <- Neo4jHelpers.getRepoSizeChart
	return lc

getSocialGraph :: String -> ApiHandler SocialGraph
getSocialGraph username = liftIO $ do
	putStrLn ("Fetching SocialGraph")
	sg <- Neo4jHelpers.getSocialGraph (DT.pack username)
        return sg

getUserBubbleChart :: String -> ApiHandler UserBubbleChart
getUserBubbleChart language = liftIO $ do
	putStrLn ("Fetching UserBubbleChart")
	sg <- Neo4jHelpers.getUserBubbleChart (DT.pack language)
        return sg

getLocationBubbleChart :: String -> ApiHandler UserBubbleChart
getLocationBubbleChart language = liftIO $ do
	putStrLn ("Fetching LocationBubbleChart")
	sg <- Neo4jHelpers.getLocationBubbleChart (DT.pack language)
        return sg

getCompanyBubbleChart :: String -> ApiHandler UserBubbleChart
getCompanyBubbleChart language = liftIO $ do
	putStrLn ("Fetching CompanyBubbleChart")
	sg <- Neo4jHelpers.getCompanyBubbleChart (DT.pack language)
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

