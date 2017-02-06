{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Minimal Multifile"
    [whamlet|
        <div class="container">
			<div class="jumbotron">
				<hi> You must login
				<p> Please Login using Github

				<div href="@{AuthR githubURL}">
					<button type="button" class="btn btn-primary" value="Login via Github">
						<i class="fa fa-github">
							<span stryle="paadding-left:3px;">GitHub
			<div>
				<pre>
					#{show sess}
    |]
