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
import Neo4jHelpers
import Database.MongoDB
import Data.ByteString.Char8 as DBC hiding (unpack, putStrLn, find)
import qualified GitHub.Endpoints.Repos as Github
import qualified GitHub.Endpoints.Users as GithubUsers
import GitHub as MainGitHub
import GitHub.Data as GHD
import GitHub.Data.Repos as GHDR
import GitHub.Data.Name as GHDN
import qualified Data.Map as M hiding (split)
import Prelude
import GitHub.Data.URL
import Data.Time.Clock

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

getJust :: Maybe Text -> IO(Text)
getJust just = liftIO $ do
	case just of
		(Just r) -> return r
		Nothing -> return (DT.pack "NAN")

getJustInt :: Maybe Int -> IO(Int)
getJustInt just = liftIO $ do
	case just of
		(Just r) -> return r
		Nothing -> return (0)
getrepos :: Text -> Maybe GHD.Auth -> State -> Int -> IO()
getrepos uname auth state hops = liftIO $ do
  	  putStrLn ("Num hops := " DL.++ (show hops))
  	  case hops > 0 of
  	  	True -> do
  	  	  putStrLn (unpack uname)
  	  	  seen_already <- atomically $ lookupNode state uname
		  case seen_already of
		    Just u -> putStrLn "User Details already stored"
		    Nothing -> do
	  	  	  let namer = (GHDN.N uname)
	  	  	  userdetails <- GithubUsers.userInfoFor' auth namer
	  	  	  case userdetails of
	  	  	  	(Left error) -> do
	  	  	  		putStrLn $ show error
	  	  	  		return ()
	  	  	  	(Right userDetails) -> do
	  	  	  		userrName <- (getJust (userName userDetails))
	  	  	  		userrCompany <- (getJust (userCompany userDetails))
	  	  	  		userrLocation <- (getJust (userLocation userDetails))
	  	  	  		userrEmail <- (getJust (userEmail userDetails))
	  	  	  		userrBio <-  (getJust (userBio userDetails))
	  	  	  		let userData = (UserData (untagName (userLogin userDetails)) (untagId (userId userDetails)) (getUrl (userUrl userDetails)) userrName userrCompany userrLocation userrEmail userrBio (userPublicRepos userDetails) (userPublicGists userDetails) (userFollowers userDetails) (userFollowing userDetails))                            
	  	  	  	        stored <- storeUserNodeNeo userData
	  	  	  	        case stored of
	  	  	  	        	False -> putStrLn "False"
	  	  	  	        	True -> putStrLn "True"
	  	  	  	        let userData = (Node (unpack uname) "User")    	
	  	  	  	        atomically $ addNode state userData uname
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
        let repooOwner = untagName $ simpleOwnerLogin (repoOwner repo)
	case repooOwner == uname of
	  	  True -> do
	    		link <- storeOwnerLinkNeo uname repoHTML
	    		case link of
	  		  	  False -> putStrLn ("Failed to store link: " ++ (DT.unpack uname) ++ " is_owner " ++ (DT.unpack repoHTML))
  			  	  True -> putStrLn ("Stored link: " ++ (DT.unpack uname) ++ " is_owner " ++ (DT.unpack repoHTML))
	  	  False -> putStrLn "Not Owner"
    	return ()
    Nothing -> do	
      repooDescription <- getJust (repoDescription repo)
      repooForks <- getJustInt (repoForks repo)
      repooSize <- getJustInt (repoSize repo)
      repooWatchers <- getJustInt (repoWatchers repo)
      repooOpenIssues <- getJustInt (repoOpenIssues repo)
      let repoData = (RepoData (floor $ utctDayTime (fromJust (repoUpdatedAt repo)) :: Int) (untagId $ repoId repo) (untagName $ simpleOwnerLogin (repoOwner repo)) (untagId $ simpleOwnerId (repoOwner repo)) (untagName $ (repoName repo)) repooDescription (repoPrivate repo) (getUrl $ repoHtmlUrl repo) repooForks (repoStargazersCount repo) repooWatchers repooSize repooOpenIssues (floor $ utctDayTime (fromJust (repoPushedAt repo)) :: Int) (floor $ utctDayTime (fromJust (repoCreatedAt repo)) :: Int) (floor $ utctDayTime (fromJust (repoUpdatedAt repo)) :: Int))
      stored <- storeRepoNodeNeo repoData
      case stored of
      	False -> putStrLn ("REPO: " ++ (DT.unpack repoHTML) ++ " Did not store successfully")
      	True -> putStrLn ("REPO: " ++ (DT.unpack repoHTML) ++ " Did store successfully")
      let repooOwner = untagName $ simpleOwnerLogin (repoOwner repo)
      case repooOwner == uname of
	  	True -> do
	  		link <- storeOwnerLinkNeo uname repoHTML
	  		case link of
			  	False -> putStrLn ("Failed to store link: " ++ (DT.unpack uname) ++ " is_owner " ++ (DT.unpack repoHTML))
			  	True -> putStrLn ("Stored link: " ++ (DT.unpack uname) ++ " is_owner " ++ (DT.unpack repoHTML))
	  	False -> putStrLn "Not Owner"

      --liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["id" =: (unpack uname)] "Node_RECORD") $ toBSON user
      --let link = (Link (unpack uname) (unpack repoHTML) "1")
      --liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["source" =: (unpack uname), "target" =: (unpack repoHTML)] "Link_RECORD") $ toBSON link	
      let language = formatLanguage (GHDR.repoLanguage repo)
      seen_already <- atomically $ lookupNode state language
      case seen_already of
      	Just seen -> do
      		link <- storeLanguagelink language repoHTML
      		case link of
      			False -> putStrLn ("Could not store link: " ++ (DT.unpack repoHTML) ++ " Written in " ++ (DT.unpack language))
      			True -> putStrLn ("Stored link: " ++ (DT.unpack repoHTML) ++ " Written in " ++ (DT.unpack language))
      	Nothing -> do
      		let nodeData = (Node (unpack language) (unpack language))
      		atomically $ addNode state nodeData language
      		stored <- storeLanguageNode language
      		link <- storeLanguagelink language repoHTML
      		case link of
      			False -> putStrLn ("Could not store link: " ++ (DT.unpack repoHTML) ++ " Written in " ++ (DT.unpack language))
      			True -> putStrLn ("Stored link: " ++ (DT.unpack repoHTML) ++ " Written in " ++ (DT.unpack language))
      putStrLn (unpack repoHTML)
      let repodata = Node (unpack repoHTML) (unpack language)
      atomically $ addNode state repodata repoHTML
      --liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["id" =: (unpack repoHTML)] "Node_RECORD") $ toBSON repodata
      contributers <- Github.contributors' auth (mkOwnerName $ untagName $ simpleOwnerLogin (GHDR.repoOwner repo)) (GHDR.repoName repo)
      case contributers of
        (Left error) -> putStrLn $ "Error: " DL.++ (show error)
        (Right contribs) -> mapM_ (userformat auth state repoHTML hops) contribs

userformat :: Maybe GHD.Auth -> State -> Text -> Int -> GHDR.Contributor -> IO()
userformat auth state prev_repo hops contributer@(Github.KnownContributor contributions _ _ _ _ _) = do
  --let contrib = contributer
  let userrLogin = untagName $ simpleUserLogin $ fromJust $ contributorToSimpleUser contributer
  putStrLn (unpack userrLogin)
  let userData = (Node (unpack userrLogin) "User")
   
  --let link = (Link (unpack userLogin) (unpack prev_repo) (show contributions))
  --liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["source" =: (unpack userLogin), "target" =: (unpack prev_repo)] "Link_RECORD") $ toBSON link
  seen_already <- atomically $ lookupNode state userrLogin
  case seen_already of
    Just u -> do
        link <- storeCollabLinkNeo userrLogin prev_repo
        case link of
  	        False -> putStrLn ("Failed to store link: " ++ (DT.unpack userrLogin) ++ " is_collab " ++ (DT.unpack prev_repo))
  	        True -> putStrLn ("Stored link: " ++ (DT.unpack userrLogin) ++ " is_collab " ++ (DT.unpack prev_repo))
    	return ()
    Nothing -> do
      let namer = (GHDN.N userrLogin)
      userdetails <- GithubUsers.userInfoFor' auth namer
      case userdetails of
  	  	(Left error) -> do
  	  		putStrLn $ show error
  	  		return ()
  	  	(Right userDetails) -> do
  	  		userrName <- (getJust (userName userDetails))
  	  		userrCompany <- (getJust (userCompany userDetails))
  	  		userrLocation <- (getJust (userLocation userDetails))
  	  		userrEmail <- (getJust (userEmail userDetails))
  	  		userrBio <-  (getJust (userBio userDetails))
  	  		let userData = (UserData (untagName (userLogin userDetails)) (untagId (userId userDetails)) (getUrl (userUrl userDetails)) userrName userrCompany userrLocation userrEmail userrBio (userPublicRepos userDetails) (userPublicGists userDetails) (userFollowers userDetails) (userFollowing userDetails))                            
  	  	        stored <- storeUserNodeNeo userData
  	  	        case stored of
  	  	        	False -> putStrLn "False"
  	  	        	True -> putStrLn "True"
      --liftIO $ MongodbHelpers.withMongoDbConnection $ upsert (select ["id" =: (unpack userLogin)] "Node_RECORD") $ toBSON userData
      link <- storeCollabLinkNeo userrLogin prev_repo
      case link of
  	    False -> putStrLn ("Failed to store link: " ++ (DT.unpack userrLogin) ++ " is_collab " ++ (DT.unpack prev_repo))
  	    True -> putStrLn ("Stored link: " ++ (DT.unpack userrLogin) ++ " is_collab " ++ (DT.unpack prev_repo))
      atomically $ addNode state userData userrLogin
      getrepos userrLogin auth state hops
      return ()

formatLanguage (Just language) = getLanguage language
formatLanguage Nothing = DT.pack ""



