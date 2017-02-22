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

module Crawler where
import System.Random
import           Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           Data.List.Split
import           GHC.Generics
import           Network.Wai hiding(Response)
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import Control.Concurrent
import           Servant
import Data.Text as DT hiding (find)
import           Servant.API
import           Servant.Client
import           System.IO
import           System.Directory
import           	System.Environment           (getArgs, getProgName, lookupEnv)
import           	System.Log.Formatter
import           	System.Log.Handler           (setFormatter)
import           	System.Log.Handler.Simple
import           	System.Log.Handler.Syslog
import           	System.Log.Logger
import         	  Data.Bson.Generic
import qualified Data.List as DL hiding (find)
import Data.Maybe                   (catMaybes, fromJust)
import Control.Monad (when, liftM)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import CommonResources
import MongodbHelpers
import Database.MongoDB
import Data.ByteString.Char8 as DBC hiding (unpack, putStrLn, find)
import qualified GitHub.Endpoints.Repos as Github
import GitHub as MainGitHub
import GitHub.Data as GHD
import GitHub.Data.Repos as GHDR
import qualified Data.Map as M hiding (split)
import Prelude
import GitHub.Data.URL

type ApiHandler = ExceptT ServantErr IO

data Task = Task{
	tskUsername :: String,
	tskAuthToken :: String,
	running :: Bool
}deriving(ToJSON, ToBSON, FromJSON, FromBSON, Generic, Show, Eq)

instance ToBSON Bool
instance FromBSON Bool

data State = State{
  nodes :: TVar (M.Map Text Node)
}

newState :: IO State
newState = atomically $ do
    State <$> newTVar M.empty

addNode :: State -> Node -> Text -> STM ()
addNode State{..} repodata repourl = do
    modifyTVar nodes . M.insert repourl $ repodata

lookupNode :: State -> Text -> STM (Maybe Node)
lookupNode State{..} repourl = M.lookup repourl <$> readTVar nodes

crawlerApi :: Proxy CrawlerApi
crawlerApi = Proxy

server :: Server CrawlerApi
server = 
    initialize :<|>
    terminate

crawlerApp :: Application
crawlerApp = serve crawlerApi server

mkApp :: IO()
mkApp = do
    putStrLn ("Crawler starting on port: " ++ crawlerport)
    run (read (crawlerport) ::Int) crawlerApp 

initialize :: CommonResources.User -> ApiHandler Response
initialize (CommonResources.User username authToken) = liftIO $ do
	putStrLn "Received user crawl request, storing request"
	let task = (Task username authToken True)
	MongodbHelpers.withMongoDbConnection $ upsert (select ["tskUsername" =: username] "USER_TASK_RECORD") $ toBSON task
	state <- liftIO $ newState
	let auth = Just $ MainGitHub.OAuth $ (DBC.pack authToken)
        liftIO $ forkIO $ getrepos (DT.pack username) auth state Nothing
        let resp = (Response "Started")
        return resp

terminate :: CommonResources.User -> ApiHandler Response
terminate (CommonResources.User username authToken) = liftIO $ do
	putStrLn "Terminating User Task"
	let task = (Task username authToken False)
	MongodbHelpers.withMongoDbConnection $ upsert (select ["tskUsername" =: username] "USER_TASK_RECORD") $ toBSON task
	let resp = (Response "Terminated")
	return resp

getrepos :: Text -> Maybe GHD.Auth -> State -> Maybe Text -> IO()
getrepos uname auth state prev_repo = liftIO $ do
  tasker <- MongodbHelpers.withMongoDbConnection $ do
  	docs <- find (select ["tskUsername" =: (unpack uname)] "USER_TASK_RECORD") >>= drainCursor
  	return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Task) docs
  case tasker of
  	[(Task _ _ False)] -> return ()
  	[(Task _ _ True)] -> do
	  possibleRepos <- Github.userRepos (mkOwnerName uname) GHDR.RepoPublicityAll
	  case possibleRepos of
	       (Left error)  -> return ()
	       (Right repos) -> do
	          mapM_ (formatRepo auth uname state prev_repo) repos
	         --return (V.toList x)

formatRepo :: Maybe GHD.Auth -> Text -> State -> Maybe Text -> Github.Repo -> IO()
formatRepo auth uname state prev_repo repo = do
  let repoHTML = getUrl $ GHDR.repoHtmlUrl repo
  seen_already <- atomically $ lookupNode state repoHTML
  case seen_already of
    Just r -> return ()
    Nothing -> do
      case prev_repo of 
        Just r -> do
          let link = (Link (unpack r) (unpack repoHTML) "Contributor")
          liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["start" =: (unpack r), "target" =: (unpack repoHTML)] "Link_RECORD") $ toBSON link
        Nothing -> putStrLn "Unseen"
      let repodata = Node (unpack repoHTML) "Repo"
      atomically $ addNode state repodata repoHTML
      liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["name" =: (unpack repoHTML)] "Node_RECORD") $ toBSON repodata
      contributers <- Github.contributors' auth (mkOwnerName $ untagName $ simpleOwnerLogin (GHDR.repoOwner repo)) (GHDR.repoName repo)
      case contributers of
        (Left error) -> putStrLn $ "Error: " DL.++ (show error)
        (Right contribs) -> mapM_ (userformat auth state (Just repoHTML)) contribs

userformat :: Maybe GHD.Auth -> State -> Maybe Text -> GHDR.Contributor -> IO()
userformat auth state prev_repo contributer = do
  let userLogin = untagName $ simpleUserLogin $ fromJust $ contributorToSimpleUser contributer
  let userData = (Node (unpack userLogin) "User")
  seen_already <- atomically $ lookupNode state userLogin
  case seen_already of
    Just u -> return ()
    Nothing -> do
      liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["name" =: (unpack userLogin)] "Node_RECORD") $ toBSON userData
      atomically $ addNode state userData userLogin
      getrepos userLogin auth state prev_repo

fotmatLanguage (Just language) = getLanguage language
formatLanguage Nothing = ""


