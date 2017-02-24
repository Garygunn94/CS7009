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
	"socialGraph" :> ReqBody '[JSON] User :> Post '[JSON] SocialGraph

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

data SocialGraph = SocialGraph{
	nodes :: [Node],
	links :: [CommonResources.Link]
}deriving(ToJSON, FromJSON, Generic, Eq, Show)

data User = User{
	username :: String,
	auth_token :: String
}deriving(ToJSON, FromJSON, Generic, Show, Eq)

data Response = Response{
	respString :: String
}deriving(ToJSON, FromJSON, Generic, Eq, Show)