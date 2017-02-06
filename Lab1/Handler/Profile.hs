{-# LANGUAGE OverloadedStrings #-}
module Handler.Profile where

import Import hiding (unpack, pack)
import Data.List hiding(intercalate, map, lookup)
import qualified GitHub.Endpoints.Repos as Github
import GitHub.Data as GHD
import GitHub.Data.Repos as GHDR
import Data.Maybe
import Data.Text.Encoding
import Data.Text hiding(intercalate, map, lookup)

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
    	sess <- getSession
    	let access_token = lookup "access_token" sess
    	let uname = lookup "login" sess
        deets <- liftIO $ repos (Data.Text.Encoding.decodeUtf8 (fromJust uname))
        setTitle . toHtml $ Data.Text.Encoding.decodeUtf8 (fromJust uname) <> "'s User page"
        $(widgetFile "profile")

repos :: Text -> IO (String)
repos uname = do
  possibleRepos <- Github.userRepos (mkOwnerName uname) GHDR.RepoPublicityAll
  case possibleRepos of
       (Left error)  -> return $ "Error: " Data.List.++ (show error)
       (Right repos) -> return $ intercalate "\n\n" $ map formatRepo repos

formatRepo repo =
  unpack (untagName (GHDR.repoName repo)) Data.List.++ "\t" Data.List.++
    unpack (fromMaybe "" $ (GHDR.repoDescription repo)) Data.List.++ "\n" Data.List.++
    unpack (GHDR.repoHtmlUrl repo) Data.List.++ "\n" Data.List.++
    unpack (fromMaybe "" $ (GHDR.repoCloneUrl repo)) Data.List.++ "\t" Data.List.++
    --(formatDate $ Github.repoUpdatedAt repo) Data.List.++ "\n" Data.List.++
    formatLanguage (GHDR.repoLanguage repo) Data.List.++
    "watchers: " Data.List.++ (show $ GHDR.repoWatchers repo) Data.List.++ "\t" Data.List.++
    "forks: " Data.List.++ (show $ GHDR.repoForks repo)

--formatDate (Just date) = show . Github.fromDate $ date
--formatDate Nothing = ""

formatLanguage (Just language) = "language: " Data.List.++ (unpack (getLanguage language)) Data.List.++ "\t"
formatLanguage Nothing = ""
