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
import Data.Maybe                   (catMaybes, fromJust, fromMaybe)
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
initialize (CommonResources.User username authToken num_hops) = liftIO $ do
	putStrLn "Received user crawl request, storing request"
	tasker <- MongodbHelpers.withMongoDbConnection $ do
	 	docs <- find (select ["tskUsername" =: (username)] "USER_TASK_RECORD") >>= drainCursor
	  	return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Task) docs
	case tasker of
	   [(Task _ False)] -> return (Response "Already Completed")
	   [(Task _ True)] -> return (Response "Currently executing")
	   _ -> do
			let task = (Task username True)
			MongodbHelpers.withMongoDbConnection $ upsert (select ["tskUsername" =: username] "USER_TASK_RECORD") $ toBSON task
			state <- liftIO $ newState
			let auth = Just $ MainGitHub.OAuth $ (DBC.pack authToken)
		        liftIO $ forkIO $ getrepos (DT.pack username) auth state num_hops
		        let resp = (Response "Started")
		        return resp

terminate :: CommonResources.User -> ApiHandler Response
terminate (CommonResources.User username authToken num_hops) = liftIO $ do
	putStrLn "Terminating User Task"
	let task = (Task username False)
	MongodbHelpers.withMongoDbConnection $ upsert (select ["tskUsername" =: username] "USER_TASK_RECORD") $ toBSON task
	let resp = (Response "Terminated")
	return resp

getrepos :: Text -> Maybe GHD.Auth -> State -> Int -> IO()
getrepos uname auth state hops = liftIO $ do
  	  putStrLn ("Num hops := " DL.++ (show hops))
  	  case hops > 0 of
  	  	True -> do
  	  	  putStrLn (unpack uname)
		  possibleRepos <- Github.userRepos (mkOwnerName uname) GHDR.RepoPublicityAll
		  case possibleRepos of
		       (Left error)  -> do
	                putStrLn $ show error
		       	return ()
		       (Right repos) -> do
		       	  let hopers = hops - 1
		          mapM_ (formatRepo auth uname state hopers) repos
		          tasker <- MongodbHelpers.withMongoDbConnection $ do
				 	docs <- find (select ["tskUsername" =: (unpack uname)] "USER_TASK_RECORD") >>= drainCursor
				  	return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Task) docs
		          case tasker of
			     [(Task _ True)] -> do
				  let task = (Task (unpack uname) False)
				  MongodbHelpers.withMongoDbConnection $ upsert (select ["tskUsername" =: (unpack uname)] "USER_TASK_RECORD") $ toBSON task
				  return ()
			     _ -> return ()

		False -> do
			putStrLn "Completed all hops"
			return ()
	         --return (V.toList x)
	--_ -> return()

formatRepo :: Maybe GHD.Auth -> Text -> State -> Int ->  Github.Repo -> IO()
formatRepo auth uname state hops repo = do
  let repoHTML = getUrl $ GHDR.repoHtmlUrl repo
  seen_already <- atomically $ lookupNode state repoHTML
  case seen_already of
    Just r -> do
        putStrLn ((show r) DL.++ ": Repo seen already")
    	return ()
    Nothing -> do
      let user = (Node (unpack uname) "User")
      liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["id" =: (unpack uname)] "Node_RECORD") $ toBSON user
      let link = (Link (unpack uname) (unpack repoHTML) "1")
      liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["source" =: (unpack uname), "target" =: (unpack repoHTML)] "Link_RECORD") $ toBSON link	
      let language = formatLanguage (GHDR.repoLanguage repo)
      putStrLn (unpack repoHTML)
      let repodata = Node (unpack repoHTML) (unpack language)
      atomically $ addNode state repodata repoHTML
      liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["id" =: (unpack repoHTML)] "Node_RECORD") $ toBSON repodata
      contributers <- Github.contributors' auth (mkOwnerName $ untagName $ simpleOwnerLogin (GHDR.repoOwner repo)) (GHDR.repoName repo)
      case contributers of
        (Left error) -> putStrLn $ "Error: " DL.++ (show error)
        (Right contribs) -> mapM_ (userformat auth state repoHTML hops) contribs

userformat :: Maybe GHD.Auth -> State -> Text -> Int -> GHDR.Contributor -> IO()
userformat auth state prev_repo hops contributer@(Github.KnownContributor contributions _ _ _ _ _) = do
  --let contrib = contributer
  let userLogin = untagName $ simpleUserLogin $ fromJust $ contributorToSimpleUser contributer
  putStrLn (unpack userLogin)
  let userData = (Node (unpack userLogin) "User")
  let link = (Link (unpack userLogin) (unpack prev_repo) (show contributions))
  liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["source" =: (unpack userLogin), "target" =: (unpack prev_repo)] "Link_RECORD") $ toBSON link
  seen_already <- atomically $ lookupNode state userLogin
  case seen_already of
    Just u -> return ()
    Nothing -> do
      liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["id" =: (unpack userLogin)] "Node_RECORD") $ toBSON userData
      atomically $ addNode state userData userLogin
      getrepos userLogin auth state hops
      return ()

formatLanguage (Just language) = getLanguage language
formatLanguage Nothing = ""


