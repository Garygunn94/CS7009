{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CommonResources where

import            Data.Aeson
import            Data.Aeson.TH
import            Data.Bits
import            Data.Bson.Generic
import            Data.Char
import            Data.Time
import            GHC.Generics
import            Servant
import            Data.Text

crawlerport :: String
crawlerport = "8080"

crawlerhost :: String
crawlerhost = "localhost"

searchport :: String
searchport = "8081"

searchhost :: String
searchhost = "localhost"

type CrawlerApi = 
    "init" :> ReqBody '[JSON] User :> Post '[JSON] Response :<|>
    "kill" :> ReqBody '[JSON] User :> Post '[JSON] Response

type SearchApi =
	"socialGraph" :> Get '[JSON] SocialGraph :<|>
  "languageChart" :> Get '[JSON] LanguageChart

data LanguageChart = LanguageChart{
  language :: [String],
  frequency :: [Int]
} deriving(ToJSON, FromJSON, Generic, Eq, Show)

data Node = Node{
  id :: String,
  group :: String
} deriving(ToJSON, FromJSON, Generic, Eq, Show, ToBSON, FromBSON)
instance FromBSON String
instance ToBSON String

data Link = Link{
  source :: String,
  target :: String,
  value :: String
}deriving(ToJSON, FromJSON, Generic, Eq, Show, ToBSON, FromBSON)

data SocialGraph = SocialGraph{
	nodes :: [Node],
	links :: [CommonResources.Link]
}deriving(ToJSON, FromJSON, Generic, Eq, Show)

data User = User{
	username :: String,
	auth_token :: String,
  num_hops :: Int
}deriving(ToJSON, FromJSON, Generic, Show, Eq)

data Response = Response{
	respString :: String
}deriving(ToJSON, FromJSON, Generic, Eq, Show)

data UserData = UserData {
  u_login :: Text,
  u_id :: Int,
  u_url :: Text,
  u_name :: Text,
  u_company :: Text,
  u_location :: Text,
  u_email :: Text,
  u_bio :: Text,
  u_public_repos :: Int,
  u_public_gists :: Int,
  u_followers :: Int,
  u_following :: Int 
} deriving(Generic, ToJSON, Eq, Show)

-- https://developer.github.com/v3/repos/
data RepoData = RepoData {
  r_last_updated :: Int,
  r_id :: Int,
  r_owner_name :: Text,
  r_owner_id :: Int,
  -- More owner details
  r_name :: Text, -- 5
  r_description :: Text,
  r_is_private :: Bool,
  r_html_url :: Text,
  -- More urls
  r_forks :: Int, -- 10
  r_stargazers :: Int,
  r_watchers :: Int,
  r_size :: Int,
  r_open_issues :: Int,
  -- More 'has' flags
  r_pushed_at :: Int,
  r_created_at :: Int,
  r_updated_at :: Int
} deriving(Generic, ToJSON, Eq, Show)