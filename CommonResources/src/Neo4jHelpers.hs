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
{-# LANGUAGE ScopedTypeVariables #-}

module Neo4jHelpers where

import qualified Database.Bolt as Neo
import qualified Data.Map as DM
import qualified Data.Text as DT
import qualified Data.List as DL
import CommonResources


storeLanguagelink :: DT.Text -> DT.Text -> IO Bool
storeLanguagelink language repoHtml = do
  let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
  neo_pipe <- Neo.connect $ neo_conf 

  -- -- Add node
  records <- Neo.run neo_pipe $ Neo.queryP (DT.pack cypher) params

  Neo.close neo_pipe

  let isEmpty = null records
  return isEmpty

  where cypher = "MATCH (u:Repo {r_html_url: {repoHtml}}), (r:Language {language: {language}}) \n CREATE (u)-[:IS_WRITTEN_IN]->(r)" --14

        params = DM.fromList [
            ("language", Neo.T language),
            ("repoHtml", Neo.T repoHtml)]


storeOwnerLinkNeo :: DT.Text -> DT.Text-> IO Bool
storeOwnerLinkNeo userName repoName = do
  let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
  neo_pipe <- Neo.connect $ neo_conf 

  -- -- Add node
  records <- Neo.run neo_pipe $ Neo.queryP (DT.pack cypher) params

  Neo.close neo_pipe

  let isEmpty = null records
  return isEmpty

  where cypher = "MATCH (u:User {u_login: {userName}}), (r:Repo {r_html_url: {repoName}}) \n CREATE (u)-[:IS_OWNER]->(r)" --14

        params = DM.fromList [
            ("userName", Neo.T userName),
            ("repoName", Neo.T repoName)]

storeCollabLinkNeo :: DT.Text -> DT.Text-> IO Bool
storeCollabLinkNeo userName repoName = do
  let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
  neo_pipe <- Neo.connect $ neo_conf 

  -- -- Add node
  records <- Neo.run neo_pipe $ Neo.queryP (DT.pack cypher) params

  Neo.close neo_pipe

  let isEmpty = null records
  return isEmpty

  where cypher = "MATCH (u:User {u_login: {userName}}), (r:Repo {r_html_url: {repoName}}) \n CREATE (u)-[:IS_COLLABORATOR]->(r)" --14

        params = DM.fromList [
            ("userName", Neo.T userName),
            ("repoName", Neo.T repoName)]

storeLanguageNode :: DT.Text -> IO Bool
storeLanguageNode language = do
  let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
  neo_pipe <- Neo.connect $ neo_conf 

  -- -- Add node
  records <- Neo.run neo_pipe $ Neo.queryP (DT.pack cypher) params

  Neo.close neo_pipe

  let isEmpty = null records
  return isEmpty

  where cypher = "CREATE (n:Language { " ++
            " language: {language} } )" --14

        params = DM.fromList [
            ("language", Neo.T language)]


storeUserNodeNeo :: UserData -> IO Bool
storeUserNodeNeo (UserData
                    u_login
                    u_id 
                    u_url 
                    u_name 
                    u_company 
                    u_location 
                    u_email  
                    u_bio 
                    u_public_repos 
                    u_public_gists 
                    u_followers 
                    u_following
                 ) = do 

  let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
  neo_pipe <- Neo.connect $ neo_conf 

  -- -- Add node
  records <- Neo.run neo_pipe $ Neo.queryP (DT.pack cypher) params

  Neo.close neo_pipe

  let isEmpty = null records
  return isEmpty

  where cypher = "CREATE (n:User { " ++
            " u_login: {u_login}, " ++
            " u_url: {u_url}, " ++
            " u_name: {u_name}, " ++
            " u_company: {u_company}, " ++
            " u_location: {u_location}, " ++
            " u_email: {u_email}, " ++
            " u_bio: {u_bio}, " ++
            " u_public_reps: {u_public_repos}, " ++
            " u_public_gists: {u_public_gists}, " ++
            " u_followers: {u_followers}, " ++
            " u_following: {u_following} } )" --14

        params = DM.fromList [
            ("u_login", Neo.T u_login),
            ("u_id", Neo.I u_id),
            ("u_url", Neo.T u_url),
            ("u_name", Neo.T u_name),
            ("u_company", Neo.T u_company),
            ("u_location", Neo.T u_location),
            ("u_email", Neo.T u_email),
            ("u_bio", Neo.T u_bio),
            ("u_public_repos", Neo.I u_public_repos),
            ("u_public_gists", Neo.I u_public_gists),
            ("u_followers", Neo.I u_followers),
            ("u_following", Neo.I u_following)]

storeRepoNodeNeo :: RepoData -> IO Bool
storeRepoNodeNeo (RepoData
                    r_last_updated 
                    r_id 
                    r_owner_name 
                    r_owner_id 
                    r_name 
                    r_description 
                    r_is_private  
                    r_html_url 
                    r_forks 
                    r_stargazers 
                    r_watchers 
                    r_size 
                    r_open_issues 
                    r_pushed_at 
                    r_created_at 
                    r_updated_at
                 ) = do

  let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
  neo_pipe <- Neo.connect $ neo_conf
  
  -- -- Add node
  records <- Neo.run neo_pipe $ Neo.queryP (DT.pack cypher) params

  Neo.close neo_pipe
  
  let isEmpty = null records
  return isEmpty
  
  where cypher = "MERGE (n:Repo { " ++
            " r_last_updated: {r_last_updated}, " ++
            " r_id: {r_id}, " ++
            " r_owner_name: {r_owner_name}, " ++
            " r_owner_id: {r_owner_id}, " ++
            " r_name: {r_name}, " ++
            " r_description: {r_description}, " ++
            " r_is_private: {r_is_private}, " ++
            " r_html_url: {r_html_url}, " ++
            " r_forks: {r_forks}, " ++
            " r_stargazers: {r_stargazers}, " ++
            " r_watchers: {r_watchers}, " ++
            " r_size: {r_size}, " ++
            " r_open_issues: {r_open_issues}, " ++
            " r_pushed_at: {r_pushed_at}, " ++
            " r_created_at: {r_created_at}, " ++
            " r_updated_at: {r_updated_at}})"

        params = DM.fromList [
            ("r_last_updated", Neo.I r_last_updated),
            ("r_id", Neo.I r_id),
            ("r_owner_name", Neo.T r_owner_name),
            ("r_owner_id", Neo.I r_owner_id),
            ("r_name", Neo.T r_name),
            ("r_description", Neo.T r_description),
            ("r_is_private", Neo.B r_is_private),
            ("r_html_url", Neo.T r_html_url),
            ("r_forks", Neo.I r_forks),
            ("r_stargazers", Neo.I r_stargazers),
            ("r_watchers", Neo.I r_watchers),
            ("r_size", Neo.I r_size),
            ("r_open_issues", Neo.I r_open_issues),
            ("r_pushed_at", Neo.I r_pushed_at),
            ("r_created_at", Neo.I r_created_at),
            ("r_updated_at", Neo.I r_updated_at)]

--getRepoByNameNeo :: Text -> IO RepoData
getRepoByFullNameNeo :: DT.Text -> IO DT.Text
--getRepoByNameNeo :: Text -> IO [Text]
getRepoByFullNameNeo r_name = do

  let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
  neo_pipe <- Neo.connect $ neo_conf
  
  -- -- Add node
  records <- Neo.run neo_pipe $ Neo.queryP (DT.pack cypher) params

  Neo.close neo_pipe
  
  -- TODO: Check for empty (case)
  --repo <- recordToRepoData $ DL.head records

  --return repo
  --return (DM.keys $ DL.head records)
  let r = DL.head records
  n <- recordToNode r
  rd <- nodeToRepoData n
  return (DT.pack (show rd))
  
  where cypher = "MATCH (n:Repo) WHERE" ++
            " n.r_full_name = {r_full_name}" ++
            " RETURN n" ++
            " LIMIT 1"

        params = DM.fromList [
            ("r_name", Neo.T r_name)
            ]

recordToNode :: Neo.Record -> IO Neo.Node
recordToNode r = do
  node :: Neo.Node <- (r `Neo.at` "n") >>= Neo.exact
  return node
 
nodeToRepoData :: Neo.Node -> IO RepoData
nodeToRepoData node = do
  let props = Neo.nodeProps node

  r_last_updated :: Int  <- (props `Neo.at` "r_last_updated") >>= Neo.exact
  r_id :: Int <- (props `Neo.at` "r_id") >>= Neo.exact
  r_owner_name :: DT.Text <- (props `Neo.at` "r_owner_name") >>= Neo.exact
  r_owner_id :: Int <- (props `Neo.at` "r_owner_id") >>= Neo.exact
  r_name :: DT.Text <- (props `Neo.at` "r_name") >>= Neo.exact
  r_description :: DT.Text <- (props `Neo.at` "r_description") >>= Neo.exact
  r_is_private :: Bool <- (props `Neo.at` "r_is_private") >>= Neo.exact
  r_is_fork :: Bool <- (props `Neo.at` "r_is_fork") >>= Neo.exact
  r_html_url :: DT.Text <- (props `Neo.at` "r_html_url") >>= Neo.exact
  r_forks :: Int <- (props `Neo.at` "r_forks") >>= Neo.exact
  r_stargazers :: Int <- (props `Neo.at` "r_stargazers") >>= Neo.exact
  r_watchers :: Int <- (props `Neo.at` "r_watchers") >>= Neo.exact
  r_size :: Int <- (props `Neo.at` "r_size") >>= Neo.exact
  r_open_issues :: Int <- (props `Neo.at` "r_open_issues") >>= Neo.exact
  r_has_issues :: Bool <- (props `Neo.at` "r_has_issues") >>= Neo.exact
  r_pushed_at :: Int <- (props `Neo.at` "r_pushed_at") >>= Neo.exact
  r_created_at :: Int <- (props `Neo.at` "r_created_at") >>= Neo.exact
  r_updated_at :: Int <- (props `Neo.at` "r_updated_at") >>= Neo.exact
      
  return (RepoData
          r_last_updated
          r_id
          r_owner_name
          r_owner_id
          r_name
          r_description
          r_is_private
          r_html_url
          r_forks
          r_stargazers
          r_watchers
          r_size
          r_open_issues
          r_pushed_at
          r_created_at
          r_updated_at
         )

toNode :: Neo.Record -> IO Neo.Node
toNode r = r `Neo.at` "n" >>= Neo.exact

nodeToText :: Neo.Node -> IO DT.Text
nodeToText node = do let props = Neo.nodeProps node 
                     title :: DT.Text <- (props `Neo.at` "name") >>= Neo.exact
                     return title
