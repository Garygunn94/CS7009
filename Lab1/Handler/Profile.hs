

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
import qualified GitHub.Endpoints.Repos as Github
import GitHub as MainGitHub
import GitHub.Data as GHD
import GitHub.Data.Repos as GHDR
import Data.Maybe
import Data.Bson.Generic
import Data.Aeson
import Data.Text.Encoding
import Data.Vector as V hiding (mapM, map, mapM_)
import Data.Text hiding(intercalate, map, lookup)
import MongodbHelpers
import Database.MongoDB
import Control.Concurrent
import qualified Data.Map as M hiding (split)




data Node = Node{
  name :: String,
  node_type :: String
} deriving(ToJSON, FromJSON, Generic, Eq, Show, ToBSON, FromBSON)
instance FromBSON String
instance ToBSON String

data Link = Link{
  start :: String,
  target :: String,
  link_type :: String
}deriving(ToJSON, FromJSON, Generic, Eq, Show, ToBSON, FromBSON)

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


getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    state <- liftIO $ newState
    defaultLayout $ do
    	sess <- getSession
    	let access_token = Import.lookup "access_token" sess
        let auth = Just $ MainGitHub.OAuth $ fromJust access_token
    	let uname = Import.lookup "login" sess
        let user = (Node (unpack $ Data.Text.Encoding.decodeUtf8 (fromJust uname)) "User")
        liftIO $ withMongoDbConnection $ Database.MongoDB.upsert (select ["name" =: (unpack (Data.Text.Encoding.decodeUtf8 (fromJust uname)))] "Node_RECORD") $ toBSON user
        atomically $ addNode (state) user (Data.Text.Encoding.decodeUtf8 (fromJust uname))
        liftIO $ forkIO $ getrepos (Data.Text.Encoding.decodeUtf8 (fromJust uname)) auth (state) Nothing
        setTitle . toHtml $ Data.Text.Encoding.decodeUtf8 (fromJust uname) <> "'s User page"
        $(widgetFile "profile")

getrepos :: Text -> Maybe GHD.Auth -> State -> Maybe Text -> IO()
getrepos uname auth state prev_repo = do
  possibleRepos <- Github.userRepos (mkOwnerName uname) GHDR.RepoPublicityAll
  case possibleRepos of
       (Left error)  -> return ()
       (Right repos) -> do
          mapM_ (formatRepo auth uname state prev_repo) repos
         --return (V.toList x)

formatRepo :: Maybe GHD.Auth -> Text -> State -> Maybe Text -> Github.Repo -> IO()
formatRepo auth uname state prev_repo repo = do
  --let repoName = untagName (GHDR.repoName repo)
  --let repoOwer = untagName $ simpleOwnerLogin (GHDR.repoOwner repo)
  let repoHTML = GHDR.repoHtmlUrl repo
  seen_already <- atomically $ lookupNode state repoHTML
  case seen_already of
    Just r -> return ()
    Nothing -> do
      case prev_repo of 
        Just r -> do
          let link = (Link (unpack r) (unpack repoHTML) "Contributor")
          liftIO $ withMongoDbConnection $ Database.MongoDB.upsert (select ["start" =: (unpack r), "target" =: (unpack repoHTML)] "Link_RECORD") $ toBSON link
        Nothing -> putStrLn "Unseen"
      --let user_repo_map = RepoMap (unpack uname) (unpack repoHTML)
      --liftIO $ withMongoDbConnection $ Database.MongoDB.upsert (select ["rhrepo_html" =: (unpack repoHTML), "username" =: (unpack uname)] "REPO_MAP") $ toBSON user_repo_map
      --let repoCloneHtml = fromMaybe "" $ (GHDR.repoCloneUrl repo)
      --let language = formatLanguage $ GHDR.repoLanguage repo
      let repodata = Node (unpack repoHTML) "Repo"
      atomically $ addNode state repodata repoHTML
      liftIO $ withMongoDbConnection $ Database.MongoDB.upsert (select ["name" =: (unpack repoHTML)] "Node_RECORD") $ toBSON repodata
      contributers <- Github.contributors' auth (mkOwnerName $ untagName $ simpleOwnerLogin (GHDR.repoOwner repo)) (GHDR.repoName repo)
      case contributers of
        (Left error) -> putStrLn $ pack $ "Error: " Data.List.++ (show error)
        (Right contribs) -> mapM_ (userformat auth state (Just repoHTML)) contribs

userformat :: Maybe GHD.Auth -> State -> Maybe Text -> GHDR.Contributor -> IO()
userformat auth state prev_repo contributer = do
  let userLogin = untagName $ simpleUserLogin $ fromJust $ contributorToSimpleUser contributer
  let userData = (Node (unpack userLogin) "User")
  seen_already <- atomically $ lookupNode state userLogin
  case seen_already of
    Just u -> return ()
    Nothing -> do
      liftIO $ withMongoDbConnection $ Database.MongoDB.upsert (select ["name" =: (unpack userLogin)] "Node_RECORD") $ toBSON userData
      atomically $ addNode state userData userLogin
      getrepos userLogin auth state prev_repo

        
  {-unpack (untagName (GHDR.repoName repo)) Data.List.++ "\t" Data.List.++
    unpack (fromMaybe "" $ (GHDR.repoDescription repo)) Data.List.++ "\n" Data.List.++
    unpack (GHDR.repoHtmlUrl repo) Data.List.++ "\n" Data.List.++
    unpack (fromMaybe "" $ (GHDR.repoCloneUrl repo)) Data.List.++ "\t" Data.List.++
    --(formatDate $ Github.repoUpdatedAt repo) Data.List.++ "\n" Data.List.++
    formatLanguage (GHDR.repoLanguage repo) Data.List.++
    "watchers: " Data.List.++ (show $ GHDR.repoWatchers repo) Data.List.++ "\t" Data.List.++
    "forks: " Data.List.++ (show $ GHDR.repoForks repo)

--formatDate (Just date) = show . Github.fromDate $ date
--formatDate Nothing = ""
-}
formatLanguage (Just language) = getLanguage language
formatLanguage Nothing = ""
