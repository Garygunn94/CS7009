module Handler.Profile where

import Import
import Data.List hiding(intercalate, map)
import qualified GitHub.Endpoints.Repos as Github
import Data.Maybe

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
    	sess <- getSession
    	let access_token = toList(sess) !! 1
    	let uname = toList(sess) !! 6
--        deets <- repoDetails uname
        setTitle . toHtml $ (show uname)  <> "'s User page"
        $(widgetFile "profile")

