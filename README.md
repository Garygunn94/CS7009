# CS7009 - Network Applications
Project description - Gary Gunn - 12306421

This readme will detail the functionality of the project, describing how the separate components are integrated into one web service that provides an analysis of GitHub data collected from users' GitHub accounts.

The main components in this projects are:
* Yesod based web-service*
* GitHub API Crawler*
* Neo4J graph data storage*
* MongoDB data storage*
* Search API for data extraction*

Thigs to note:
* Lab1 contains the yesod web-service*
* Javascript used contained in Lab1/templates/profile.hamlet and Lab1/templates/profile.julius*
* Code for Crawler contained in Cralwer/src/Crawler.hs*
* Code for Search contained in Search/src/Search.hs*
* Common datatypes, API definitions and functions are documented in CommonResources/src/CommonResources.hs*
* Neo4j functions for storing and retrieving information located in CommonResource/src/Neo4jHelpers.hs*

This document will focus on the usage of the yesod web-service detailing how the other components are used to present data to the user.

# Yesod Web-Service
The yesod web-service provides a user interface with which users can interact with data extracted from their github acount. When a user goes to the web-service they will be met by the splash screen seen in the screengrab below.

![](images/splash.png?raw=true)

Notice the Login button located at the top right of the web page. This login button takes you to the login screen seen below:

![](images/login.png?raw=true)

The login screen, as can be seen in the URL above is located at the endpoint named \auth\login. Clicking on the button titled 'login via github' redirects the user to a GitHub login page. The redirection to the GitHub login page is achieved by utalizing O-auth implemented in the 'Yesod.Auth.OAuth2.GitHub' library.

Pending successful login, the user is then redirected back to the original splash page, however a banner now appears to let the user know they are now logged in.

![](images/loggedin.png?raw=true)

In order to access the web-service, the user should navigate to the 'Profile' tab in order view the GitHub data analysis presented in this project.

![](images/profile.png?raw=true)

The screengrab above shows the landing page when a user navigates to the 'Profile' tab. Users will notice six buttons, all of which provide an abstraction of data crawled from their, and other users GitHub accounts. However, before more detail is provided about these analyses, the functionality behind them should be explained.

# GitHub API Cralwer
The moment a user navigates to the 'Profile' tab an API request is sent to a separate API service known as the crawler. The API for this project are all build using the servant library which provides a simple way to define APIs and integrate them into the Web-service. The API request sent to the crawler API includes the user's username and their access-token, a token recieved from the GitHub login which provides access to the GitHub API. Upon recieving this API request, the crawler API first checks a mongoDB instance to check whether this user has accessed the web-service before. The mongoDB instance is used to keep track of users and crawls of their data so as to prevent multiple crawls of the same user's GitHub. If no instance relating to this user is found in the mongoDB instance, a new one is created with a flag stating that the crawl is underway and not completed. If an instance already exists with a flag saying a crawl is underway or has been completed then a crawls of this user's GitHub data is not carried out.

Assuming the user is new to the service, a new instance will be stored in the mongoDB instance saying that a crawl of this users GitHub should now be carried out. The crawler API makes use of forked computing so that it does not hold up the yesod web-service. A fork is created for the crawl of this users data, and a response is sent back to the yesod web-service telling it that a cralw for this user is underway. Forked computing is necessary as otherwise, the web-service would have to wait for the crawl for the users data to end before the 'Profile' page could be loaded.

The Crawler API makes use of the 'GitHub' Haskell library which provides a haskell interface to the GitHub API through multiple, well defined endpoints. The crawling process can be condensed as follows:
* First, in-memory local lists of repos and users are initialised to keep track of which users and repos have been crawled previously.*
* A function, 'getrepos' recieves the username, authentification details, a reference to the previously defined lists and the number of hops to crawl away from the user.*
* This function then checks the in memory list to see if this user has already been crawled, if the user hasn't, their details are then stored as a node in the neo4j instance.*
* A list of all repos linked to this user are then collected via the GitHub API*
* For each repo in this list, the repo name, the current user, authentification details and a reference to the in-memory lists are all passed to a function named 'formatrepo'*
* In this function, the repo name is first checked against the in-memory list to see if it has been seen before, if so a link between the user and the repo is stored in the neo4j instance, if not the program continues.*
* Details about this repo, are then extracted using the GitHub API and are stored in the neo4j instance as a node.*
* Then a link between the user and repo is stored in neo4j also*
* The programming language the repo is written in is then extracted. If the language has not been seen before it is stored as a node in the neo4j instance*
* A link between the repo and the programming language is then stored in the neo4j instance.*
* Finally a list of contributers is then extracted from the repo using the GitHub API and for each contributer, authentication details, a reference to the in-memory lists, the current repo, the number of remaining hops, the language the repo is written in, and details about the contributer are sent to a function called 'userformat'*
* This function then first checks is a contributer has been seen already bu checking the in-memory lists, if the contributer has been seen before, a link is created between the contributer and the language the repo was written in, and a link between the contributer and the repo which also contains the number of commits they have made to the repo.*
* If the contributer has not been seen before, a node is created in the neo4j instance ith their details, and the same links are created between the contributer and the language/repo.*
* Then the details for this contributer are circled back to the first 'getrepos' function where the number of hops is reduced before the rest of the 'getrepos' function is executed*
* This cycle continues until the number of hops hits zero, at which point all repos and users connected to those repos within the original number of hops will have been stored in the neo4j instance. *

Once the crawl is completed, the instace relating to the original user in the mongoDB instance is updated to say that the crawlf for that user has been completed.

# Neo4J
The neo4j graph database was used due to its specialisation in modelling relationships between nodes.

![](images/neo4j.png?raw=true)

As can be seen in the screen grab above, this project makes use of three types of node; Language nodes, User nodes and Repo nodes. The relationships that connect them are as follows:
* User -> IS_OWNER -> Repo*
* User -> IS_COLLABORATOR -> Repo (This link also carries the number of commit made by the user)*
* User -> KNOWS -> Language*
* Repo -> WRITTEN_IN -> Language*

The variables relating to each of the nodes can also be seen in the screengrab above. Any variable starting with 'u_' is exclusive to a user node while any with 'r_' are exclusive to repo nodes. The 'name' variable is common to all nodes. 

In order to supply a lot of data for the analyses described later, a crawl was manually started from the d3.js GitHub repo with 4 hops. This crawl covered a lot of important repos on GitHub accross multiple languages and countries making more interesting analyses possible.

# Analyses & Search API
Social Graph:
The first analysis available for users in the 'Profile' tab is the social graph. This is a first order data abstraction from teh neo4j instance and creates a social graph between users and nodes two hops from the user using the web-service.

![](images/social_graph.png?raw=true)

The screengrab above shows my own social graph for two hops. Languages were left out as the social graph becomes too large as every user and repo stored who have links to each language will be within two hops creating a social graph too large for the web-service which causes crashing. The social graph was created using the d3.js library.

The data is retreived from the neo4j instance using a servant API known as the search API. The search API retreives all user and nodes two hops from the original user using the 'bolt' haskell library. All methods that interface with the neo4j instance are located in the common resources folder. The search API endpoint for the social graph is accessed via AJAX in the javascript running underneath the web-service. The data is returned from the search API via JSON which is then interpreted by the d3.js methods to form the social graph.   

Most popular languages:
The second analysis of the GitHub data available is a bar chart which shows the number of repos written in each language, with number of repos on the y-axis and the languages on the x-axis.

![](images/mostpop.png?raw=true)

The screengrab above shows the bar chart generated using my own crawled data as well as data collected from the crawl on the d3.js data. As can be seen in the chart, Javascript and Haskell are the two most popular language repos are written in based on the data in the neo4j instance.

This data is again obtained via the Search API which uses methods that interface with the neo4j instance located in the common resources folder. In order to make the calculations seen in this bar chart, for each language, the number of links of type "IS_WRITTEN_IN" are counted as these links model the relationship between repos and language. By counting the number of links each of these language have, a bar chart can easily be generated.

The query used to extract the bar chart data is as follows:
MATCH (n)-[r:IS_WRITTEN_IN]->(x) RETURN x.name as language, COUNT(r) as frequency ORDER BY COUNT(r) DESC LIMIT 20
This query forms a table with a column for language names and a column for the number of repos for each language.

Similarly to the social graph, the data is obtained by the web-service by using ajax in the javascript code to call the endpoint of the search API that returns the language chart. The bar chart itself is written in javascript using the d3.js library.

RepoSize - User Chart:
This scatter plot was devised to investigate whether there was a correlation between the size of repos (mb) and the number of users contributing the repo. The size of the repo is represented on the x-axis and the number of users on the y-axis

![](images/reposize.png?raw=true)

he points are different colours depending on which language the node is written in. It is clear by observing the graph that there is little to no correlation between the size of a repo and the number of users who contribute to it. Although no correlation was found, this analysis is still valuable due to the fact that it gives a very definte answer that having more users contributing to a repo does not make it larger.

The data was obtained similarly to the previous methods however, the query used to extract this information was:

MATCH (p:Language)-[s:IS_WRITTEN_IN]-(r:Repo)-[l:IS_OWNER|:IS_COLLABORATOR]-(b) RETURN r.name as Name, r.r_size as Repo_Size, count(l) as Associated_Users, p.name as language LIMIT 100

Which returns a table with a column for the name of the repo, the size of the repo, the number of users and the language the repo is written in.

AJAX and d3.js were once again used to get the information to the web-service and to display it.

Bubble Charts:
The final three analyses that users can interact with are very similar, however they present different information to the user. These charts, named bubble charts consist of circles of different sizes. For the purposes of this project they have been used to represent three abtractions of the neo4j data: Most influential user per language, most influential location per language and most influential company per language.

Most influential user per language:
For this bubble chart, each bubble represents a single GitHub user and the size of the bubbles is representitive of the number of commits each user has made to repos written in that language.

![](images/userlanguage.png?raw=true)

The screengrab above shows the most influential users for the JAVA language however more languages are available in a drop down menu. In this screengrab, the mouse is highlighting the user who appears to have the bubble with the largest radius. From this, we can conclude that this user has made the most commits to repos written in that language.

The data is extracted from the neo4j instance using the Search API. The query used to extract this data is as follows:

MATCH (p:User)-[i:IS_COLLABORATOR]-(l:Repo)-[:IS_WRITTEN_IN]-(h:Language) WHERE h.name = {language} Return p.u_login as Name, SUM(i.commits) as frequency ORDER BY SUM(i.commits) DESC LIMIT 250

Where "{language}" is the language selected from the dropdown menu which is passed to the search API when the web-service makes an AJAX call to the endpoint for the user bubble chart. The data is returned in the form of a table with a column for the username and a column for the number of commits the user has made to repos written in the selected language. 

Most influential location per language:
For this bubble chart, each bubble represents a location and the size of the bubbles is representitive of the total number of commits made by users from each location to repos written in each language.

![](images/locationbubble.png?raw=true)

The screengrab above shows the most influential locations for the JAVA language however more languages are available in a drop down menu. In this screengrab, the mouse is highlighting portugal which appears to have the bubble with the largest radius. From this, we can conclude that portugal is the location that contributes the most to repos written in Java. However, this is based purely on the data crawled from GitHub and is not necessarily representitive of the whole of GitHub.

The data is extracted from the neo4j instance using the Search API. The query used to extract this data is as follows:

MATCH (p:User)-[i:IS_COLLABORATOR]-(l:Repo)-[:IS_WRITTEN_IN]-(h:Language) WHERE h.name = {language} Return p.u_location as Name, SUM(i.commits) as frequency ORDER BY SUM(i.commits) DESC LIMIT 250

Where "{language}" is the language selected from the dropdown menu which is passed to the search API when the web-service makes an AJAX call to the endpoint for the user bubble chart. The data is returned in the form of a table with a column for the location name and a column for the number of commits users from each location have made to repos written in the selected language. 

Most influential company per language:
For this bubble chart, each bubble represents a company and the size of the bubbles is representitive of the total number of commits made by users associated with companies to repos written in each language.

![](images/companybubble.png?raw=true)

The screengrab above shows the most influential companies for the Haskell language however more languages are available in a drop down menu. In this screengrab, the mouse is highlighting Facebook which appears to be one of the bubble with the largest radius. The bigger blue bubble in the background accounts for all users who have contributed to repos written in Haskell that do not have their company listed. This was left so users can see the proportion of users who are not associated with a company.

The data is extracted from the neo4j instance using the Search API. The query used to extract this data is as follows:

MATCH (p:User)-[i:IS_COLLABORATOR]-(l:Repo)-[:IS_WRITTEN_IN]-(h:Language) WHERE h.name = {language} Return p.u_company as Name, SUM(i.commits) as frequency ORDER BY SUM(i.commits) DESC LIMIT 250

Where "{language}" is the language selected from the dropdown menu which is passed to the search API when the web-service makes an AJAX call to the endpoint for the user bubble chart. The data is returned in the form of a table with a column for the company name and a column for the number of commits users from each company have made to repos written in the selected language. 
