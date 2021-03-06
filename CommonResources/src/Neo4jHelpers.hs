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

  where cypher = "MATCH (u:Repo {r_html_url: {repoHtml}}), (r:Language {name: {language}}) \n CREATE (u)-[:IS_WRITTEN_IN]->(r)" --14

        params = DM.fromList [
            ("language", Neo.T language),
            ("repoHtml", Neo.T repoHtml)]

storeKnowslink :: DT.Text -> DT.Text -> IO Bool
storeKnowslink language userName = do
  let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
  neo_pipe <- Neo.connect $ neo_conf 

  -- -- Add node
  records <- Neo.run neo_pipe $ Neo.queryP (DT.pack cypher) params

  Neo.close neo_pipe

  let isEmpty = null records
  return isEmpty

  where cypher = "MATCH (u:User {u_login: {userName}}), (r:Language {name: {language}}) \n CREATE UNIQUE (u)-[:KNOWS]->(r)" --14

        params = DM.fromList [
            ("language", Neo.T language),
            ("userName", Neo.T userName)]


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

storeCollabLinkNeo :: DT.Text -> DT.Text-> Int -> IO Bool
storeCollabLinkNeo userName repoName commits = do
  let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
  neo_pipe <- Neo.connect $ neo_conf 

  -- -- Add node
  records <- Neo.run neo_pipe $ Neo.queryP (DT.pack cypher) params

  Neo.close neo_pipe

  let isEmpty = null records
  return isEmpty

  where cypher = "MATCH (u:User {u_login: {userName}}), (r:Repo {r_html_url: {repoName}}) \n CREATE (u)-[c:IS_COLLABORATOR {commits: {commits}}]->(r)" --14

        params = DM.fromList [
            ("userName", Neo.T userName),
            ("repoName", Neo.T repoName),
            ("commits", Neo.I commits)]

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
            " name: {language} } )" --14

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
            " name: {u_login}, " ++
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
            " name: {r_html_url}, " ++
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

getSocialGraph :: DT.Text -> IO SocialGraph
getSocialGraph username = do
  putStrLn (DT.unpack username)
  let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
  neo_pipe <- Neo.connect $ neo_conf
  
  -- -- Add node
  record_nodes <- Neo.run neo_pipe $ Neo.queryP node_cypher node_params
  record_links <- Neo.run neo_pipe $ Neo.queryP link_cypher link_params


  Neo.close neo_pipe
  
  --node_nodes <- (mapM recordToNode) record_nodes
  --node_links <- (mapM recordToNode) record_links

  nodes <- (mapM nodeToNode) record_nodes
  links <- (mapM nodeToLink) record_links


  return (SocialGraph nodes links)

  where link_cypher = "MATCH (n) WHERE n.name = {username} OPTIONAL MATCH path=(n)-[r*1..2]-(c) where NONE( rel in r WHERE type(rel)='KNOWS') WITH rels(path) AS rels UNWIND rels AS rel WITH DISTINCT rel RETURN startnode(rel).name as source, endnode(rel).name as target, type(rel) as type"
        node_params = DM.fromList [("username", Neo.T username)]
        node_cypher = "MATCH (n) WHERE n.name = {username} OPTIONAL MATCH path=(n)-[r*1..2]-(c) where NONE( rel in r WHERE type(rel)='KNOWS') RETURN DISTINCT c.name as name, HEAD(LABELS(c)) as group"
        link_params = DM.fromList [("username", Neo.T username)]

getLanguageChart :: IO LanguageChart
getLanguageChart = do
    let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
    neo_pipe <- Neo.connect $ neo_conf

    record_languageTable <- Neo.run neo_pipe $ Neo.query "MATCH (n)-[r:IS_WRITTEN_IN]->(x) RETURN x.name as language, COUNT(r) as frequency ORDER BY COUNT(r) DESC LIMIT 20"
    
    languages <- (mapM recordToLang) record_languageTable
    frequencies <- (mapM recordToFreq) record_languageTable

    return (LanguageChart languages frequencies)

getRepoSizeChart :: IO RepoSizeChart
getRepoSizeChart = do
    let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
    neo_pipe <- Neo.connect $ neo_conf

    record_RepoTable <- Neo.run neo_pipe $ Neo.query "MATCH (p:Language)-[s:IS_WRITTEN_IN]-(r:Repo)-[l:IS_OWNER|:IS_COLLABORATOR]-(b) RETURN r.name as Name, r.r_size as Repo_Size, count(l) as Associated_Users, p.name as language LIMIT 100"
    
    repos <- (mapM recordToRepoSize) record_RepoTable
    users <- (mapM recordToAssUser) record_RepoTable
    repon <- (mapM recordToRepoName) record_RepoTable
    languager <- (mapM recordToLang) record_RepoTable

    return (RepoSizeChart repos users repon languager)

getUserBubbleChart :: DT.Text -> IO UserBubbleChart
getUserBubbleChart language = do
    let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
    neo_pipe <- Neo.connect $ neo_conf

    records <- Neo.run neo_pipe $ Neo.queryP cypher params


    users <- (mapM recordToRepoName) records
    value <- (mapM recordToFreq) records

    return (UserBubbleChart users value)

    where cypher = "MATCH (p:User)-[i:IS_COLLABORATOR]-(l:Repo)-[:IS_WRITTEN_IN]-(h:Language) WHERE h.name = {language} Return p.u_login as Name, SUM(i.commits) as frequency ORDER BY SUM(i.commits) DESC LIMIT 250"
          params = DM.fromList [("language", Neo.T language)]

getLocationBubbleChart :: DT.Text -> IO UserBubbleChart
getLocationBubbleChart language = do
    let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
    neo_pipe <- Neo.connect $ neo_conf

    records <- Neo.run neo_pipe $ Neo.queryP cypher params


    users <- (mapM recordToRepoName) records
    value <- (mapM recordToFreq) records

    return (UserBubbleChart users value)

    where cypher = "MATCH (p:User)-[i:IS_COLLABORATOR]-(l:Repo)-[:IS_WRITTEN_IN]-(h:Language) WHERE h.name = {language} Return p.u_location as Name, SUM(i.commits) as frequency ORDER BY SUM(i.commits) DESC LIMIT 250"
          params = DM.fromList [("language", Neo.T language)]

getCompanyBubbleChart :: DT.Text -> IO UserBubbleChart
getCompanyBubbleChart language = do
    let neo_conf = Neo.def { Neo.user = "neo4j", Neo.password = "GaryGunn94" }
    neo_pipe <- Neo.connect $ neo_conf

    records <- Neo.run neo_pipe $ Neo.queryP cypher params


    users <- (mapM recordToRepoName) records
    value <- (mapM recordToFreq) records

    return (UserBubbleChart users value)

    where cypher = "MATCH (p:User)-[i:IS_COLLABORATOR]-(l:Repo)-[:IS_WRITTEN_IN]-(h:Language) WHERE h.name = {language} Return p.u_company as Name, SUM(i.commits) as frequency ORDER BY SUM(i.commits) DESC LIMIT 250"
          params = DM.fromList [("language", Neo.T language)]

recordToRepoSize :: Neo.Record -> IO Int
recordToRepoSize record = do
	repoSize :: Int <- (record `Neo.at` "Repo_Size") >>= Neo.exact
	--frequency :: Int <- (record `Neo.at` "frequency") >>= Neo.exact 

	return (repoSize)

recordToRepoName:: Neo.Record -> IO String
recordToRepoName record = do
	repoName :: DT.Text <- (record `Neo.at` "Name") >>= Neo.exact
	--frequency :: Int <- (record `Neo.at` "frequency") >>= Neo.exact 

	return (DT.unpack repoName)

recordToAssUser :: Neo.Record -> IO Int
recordToAssUser record = do
	users :: Int <- (record `Neo.at` "Associated_Users") >>= Neo.exact
	--frequency :: Int <- (record `Neo.at` "frequency") >>= Neo.exact 

	return users

recordToLang :: Neo.Record -> IO String
recordToLang record = do
	language :: DT.Text <- (record `Neo.at` "language") >>= Neo.exact
	--frequency :: Int <- (record `Neo.at` "frequency") >>= Neo.exact 

	return (DT.unpack language)

recordToFreq :: Neo.Record -> IO Int
recordToFreq record = do
	frequency :: Int <- (record `Neo.at` "frequency") >>= Neo.exact
	--frequency :: Int <- (record `Neo.at` "frequency") >>= Neo.exact 

	return frequency


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
 
nodeToNode :: Neo.Record -> IO CommonResources.Node
nodeToNode node = do
	--let props = Neo.nodeProps node

	name :: DT.Text <- (node `Neo.at` "name") >>= Neo.exact
	group :: DT.Text <- (node `Neo.at` "group") >>= Neo.exact 

	return (Node (DT.unpack name) (DT.unpack group))

nodeToLink :: Neo.Record -> IO CommonResources.Link
nodeToLink node = do
	--let props = Neo.nodeProps node

	source :: DT.Text <- (node `Neo.at` "source") >>= Neo.exact
	target :: DT.Text <- (node `Neo.at` "target") >>= Neo.exact
	group :: DT.Text <- (node `Neo.at` "type") >>= Neo.exact

	return (Link (DT.unpack source) (DT.unpack target) (DT.unpack group))

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
