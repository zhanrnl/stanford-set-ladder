{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Views where

import Prelude hiding (head, id, div, span)
import qualified Prelude as P
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Internal (Attributable, textValue)
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid ((<>))
import Control.Monad
import Data.Time
import System.Locale (defaultTimeLocale)
import Text.Hamlet

import Types
import Ratings

maybeWhen :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
maybeWhen maybeA f =
  case maybeA of
    Nothing -> return ()
    Just a' -> f a'

navbarEntries :: [NavbarEntry]
navbarEntries = [
  NavbarEntry Home "Home" "/",
  NavbarEntry Profile "Your profile" "/profile",
  NavbarEntry Friends "Friends" "/friends",
  NavbarHeader "Play SET!",
  NavbarEntry ReportOffline "Report offline 1v1 game" "/reportoffline",
  NavbarEntry ViewLadder "View 1v1 ladder" "/ladder"]

(!.) :: Attributable h => h -> AttributeValue -> h
tag !. val = tag ! A.class_ val
(!#) :: Attributable h => h -> AttributeValue -> h
tag !# val = tag ! A.id val
(!+) :: Attributable h => h -> AttributeValue -> h
tag !+ val = tag ! A.type_ val
(!=) :: Attributable h => h -> AttributeValue -> h
tag != val = tag ! customAttribute "data-bind" val

stylesheets :: [Text]
stylesheets = map ("/static/" <>) [
  "css/bootstrap.min.css",
  "css/darkstrap.css",
  "css/jquery-ui-1.9.0.custom.min.css",
  "font/fontface.css",
  "css/setladder.css"]
              
javascripts :: [Text]
javascripts = map ("/static/js/" <>) [
  "json2.js",
  "jquery-1.8.1.min.js",
  "jquery-ui-1.9.0.custom.min.js",
  "bootstrap.min.js",
  "knockout-2.1.0.js"]

staticIncludes :: Html
staticIncludes = [shamlet|
  $forall ss <- stylesheets
    <link rel="stylesheet" href="#{ss}">
  $forall js <- javascripts
    <script src="#{js}">
  |]

baseTemplate :: Text -> Maybe Html -> Html -> Html
baseTemplate titleText maybeHeadHtml contentHtml = [shamlet|
  $doctype 5
  <head>
    <title>#{titleText}
    #{staticIncludes}
    $maybe headHtml <- maybeHeadHtml
      #{headHtml}
    $nothing
  <body>
    #{contentHtml}
  |]

pageTemplate :: Text -> Maybe Html -> Text -> Html -> Html
pageTemplate titleText maybeHeadHtml username contentHtml =
  baseTemplate titleText maybeHeadHtml [shamlet|
    <div class="container">
      <div class="row header">
        <div class="span9">
          <a href="/">
            <div class="headerLogo smallCorners dropShadow pull-left">
          <h1 class="logoText">the Stanford Set Ladder
        $if not (username == "")
          <div class="span3 alignRight userBox">
            <p>Logged in as: 
              <strong> #{username}
            <a class="btn btn-small btn-inverse" href="/logout">Log out
      <div class="row">
        <div class="span12">
          #{contentHtml}
    |]

navHtml :: PageName -> Html
navHtml page =
  let entryLi navEntry =
        if pageName navEntry == page
        then li !. "active subtleDropShadow" $ entryContent navEntry
        else li $ entryContent navEntry
      entryContent navEntry = a ! A.href (textValue $ linkAddress navEntry) $
                              toHtml $ displayText navEntry
  in [shamlet|
  <ul class="nav nav-pills nav-stacked"> 
    $forall navEntry <- navbarEntries
      $case navEntry
        $of NavbarHeader text 
          <li class="nav-header">#{text}
        $of _
          #{entryLi navEntry}
  |]

pageTemplateNav :: PageName -> Text -> Maybe Html -> Text -> Html -> Html
pageTemplateNav page titleText maybeHeadHtml username contentHtml =
  pageTemplate titleText maybeHeadHtml username $ [shamlet|
    <div class="row">
      <div class="span3">#{navHtml page}
      <div class="span9">#{contentHtml}
    |]

index :: Text -> Html
index username = pageTemplateNav Home "Set Ladder: Home" Nothing username $ [shamlet|
  <h1 class="page-header">Welcome!
  <p>The Stanford Set Ladder is a player ranking database and system for competitive SET (the card game). Currently you can play games in real life and report the results to the Set Ladder and your and your opponent's ratings will be updated.
  <h3>Exciting features on the way
  <p>Eventually, you will be able to play games online in realtime against opponents, with an automatic matchmaking system. You will also be able play the SET Daily Puzzle and play through a full deck solitaire style to compete for the fastest time.
  |]

preEscapedText :: Text -> Html
preEscapedText = preEscapedToHtml

reportGame :: Text -> Text -> Html
reportGame username message = pageTemplateNav ReportOffline "Set Ladder: Report offline game" scripts username $ [shamlet|
  <h1 class="page-header">Report an Offline 1v1 Game
  <p>Report a game played offline (not on this site) between you and someone who has added you as a friend.
  $if message == "invalid"
    <div class="alert alert-error">
      #{closeButton}
      <strong>Invalid data sent to server!
      Perhaps the user you entered has not added you as a friend?
  <form action="/doreport" method="POST">
    <table class="report-game-form-table">
      <tbody>
        <tr>
          <th>Players: 
          <td>#{username}
          <td>vs.
          <td>
            <input type="text" id="OpponentInput" placeholder="Opponent username" 
             style="width: 140px" data-bind="value: opponent, valueUpdate: 'afterkeydown'" 
             name="opponentname" autofocus="autofocus">
        <tr data-bind="css: {error: (isNaN(ownScoreValid()) && ownScore().length > 0) || (isNaN(opponentScoreValid()) && opponentScore().length > 0) || (!numbersValid() && ownScore().length > 0 && opponentScore().length > 0)}">
          <th>Result: 
          <td>
            <input type="text" id="YourScore" style="width: 40px"
             data-bind="value: ownScore, valueUpdate: 'afterkeydown'" 
             name="ownscore"> sets
          <td>
          <td>
            <input type="text" id="OpponentScore" style="width: 40px"
             data-bind="value: opponentScore, valueUpdate: 'afterkeydown'" 
             name="opponentscore"> sets
    <span class="help-block" data-bind="text: ownScoreErrorText, visible: ownScoreErrorText().length > 0">
    <span class="help-block" data-bind="text: opponentScoreErrorText, visible: opponentScoreErrorText().length > 0">
    <span class="help-block" data-bind="text: numbersValidText, visible: numbersValidText().length > 0">
    <input type="submit" class="btn" 
     data-bind="enable: submitButtonState(), buttonEnable: submitButtonState()" 
     value="Report game">
  |]
  where scripts = Just $ script ! A.src "/static/js/reportgame.js" $ ""

friends :: Text -> [Text] -> [Text] -> Html
friends username outFriends inFriends = pageTemplateNav Friends "Set Ladder: Friends" scripts username $ [shamlet|
  <h1 class="page-header">Friends
  <p>Adding friends makes it easier to keep track of their ratings and game records, and also allows them to report offline 1v1 games between the two of you. Simply type their usernames below to add them to your list of friends.
  <form class="form-inline" action="javascript:void(0)">
    <input type="text" id="FriendNameInput"
     name="friendname" placeholder="Friend name"
     data-bind="value: friendName, valueUpdate: 'afterkeydown'">
    <button id="AddFriendButton" class="btn btn-success"
     data-bind="click: addFriend">Add friend
  <div id="ErrorContainer">
  <h3>Users you have added as a friend
  <div class="modal hide fade" id="UnfriendModal" tabindex="-1" role="dialog">
    <div class="modal-header">
      #{modalCloseButton}
      <h3>Unfriending
    <div class="modal-body">
      <p>
        Do you really want to unfriend 
        <span data-bind="text: unfriendUsername">
        ?
    <div class="modal-footer">
      <button class="btn" data-dismiss="modal">Cancel
      <button class="btn btn-primary" data-bind="click: doUnfriend" 
       data-dismiss="modal">Unfriend
  <table class="table table-condensed" data-bind="visible: outFriends().length > 0">
    <thead>
      <tr><th>Username
    <tbody>
      #{preEscapedText "<!-- ko foreach: outFriends -->"}
      <tr>
        <td data-bind="text: $data">
        <td>
          <a data-bind="attr: {href: '/profile/' + $data}">View profile
        <td>
          <a href="#UnfriendModal" data-bind="click: $root.setUnfriendUsername($data)"
           data-toggle="modal">Unfriend
      #{preEscapedText "<!-- /ko -->"}
  <p data-bind="visible: outFriends().length == 0">No friends added yet.
  <h3>Users who have added you as a friend
  $if null inFriends
    <p>No one has added you as a friend yet.
  $else
    <p>Since these users have given you permission, you can report offline 1v1 games between you and anyone on this list to update your rankings.
    <table class="table table-condensed">
      <thead>
        <tr><th>Username
      <tbody>
        $forall friendName <- inFriends
          <tr>
            <td>#{friendName}
            <td>
              <a href="#{profileLink friendName}">View profile
  |]
  where scripts = Just $ do
          script $ toHtml $
            "var outFriendsInit = [" <>
            T.intercalate ", " (map (T.pack . show) outFriends) <> "];"
          script ! A.src "/static/js/friends.js" $ ""
        profileLink = ("/profile/" <>)

viewLadder :: Text -> [(Text, Maybe Rating)] -> Html
viewLadder username userRatings = pageTemplateNav ViewLadder "Set Ladder: 1v1 Ladder" Nothing username $ [shamlet|
  <h1 class="page-header">The 1v1 Ladder
  <table class="table table-condensed">
    <thead>
      <tr>
        <th>Rank
        <th>Username
        <th>Rating
    <tbody>
      $forall ratingObj <- userRatingsRanks
        $with name <- fst $ fst ratingObj
          $with ratingM <- snd $ fst ratingObj
            $with rank <- snd ratingObj
              <tr class=#{highlightName name username}>
                <td>
                  $if isJust ratingM
                    #{rank}
                <td>#{name}
                <td>
                  $case ratingM
                    $of Just rating
                      #{rating}
                    $of Nothing
                      <span class="muted">(none)
                <td>
                  <a href="#{profileLink name}">View profile
  |]        
  where userRatingsRanks = zip userRatings ([1..] :: [Int])
        highlightName :: Text -> Text -> Text
        highlightName name self = if name == self then "highlight" else ""
        profileLink = ("/profile/" <>)

userProfile :: Text -> Text -> Text -> Text -> Maybe Rating -> [GameRecordDisplay] -> Text -> Html
userProfile self username realname location rating recentGames message =
  let isSelfProfile = (self == username)
      title = if isSelfProfile
              then "Set Ladder: Your profile"
              else ("Set Ladder: Profile of " <> username)
      page = if isSelfProfile
             then Profile
             else Other
  in pageTemplateNav page title Nothing self $ [shamlet|
  <h1 class="page-header">User profile of #{username}
  <p>
    <strong>Real name:
    #{toHtmlNoneGiven realname}
  <p>
    <strong>Location:
    #{toHtmlNoneGiven location}
  $if (message == "reportsuccess")
    <div class="alert alert-success">
      #{closeButton}
      <strong>Offline game report successful!
  <div class="row">
    <div class="span3">
      <h3>1v1 Rating
      $if isJust rating
        <div class="rating">#{fromJust rating}
      $else 
        #{toHtmlNoneGiven ""}
    <div class="span6">
      <h3>Recent 1v1 games
      $if null recentGames
        #{toHtmlNoneGiven ""}
      $else 
        <table class="table table-condensed">
          <thead>
            <tr>
              <th>Versus
              <th>Result
              <th>Rating
              <th>Date (GMT)
          <tbody>
            $forall gameRecord <- recentGames
              <tr>
                <td>
                  <a href=#{"/profile/" <> grVersus gameRecord}>#{grVersus gameRecord}
                <td>
                  #{toResult (grOwnScore gameRecord) (grOpponentScore gameRecord)}
                  #{grOwnScore gameRecord}-#{grOpponentScore gameRecord}
                <td>
                  #{grRating gameRecord}
                <td>
                  #{formatTime defaultTimeLocale "%-D %-R" (grTime gameRecord)}
  |]
  where toResult ownScore opponentScore
          | ownScore > opponentScore = span !. "text-success" $ "Win "
          | opponentScore > ownScore = span !. "text-info" $ "Lose "
          | otherwise = "Tie "

toHtmlNoneGiven :: Text -> Html
toHtmlNoneGiven t =
  if t == ""
  then span !. "muted" $ "(none)"
  else toHtml t

closeButton :: Html
closeButton =
  button !. "close" !+ "button" ! customAttribute "data-dismiss" "alert" $
  preEscapedText "&times;"
  
modalCloseButton :: Html
modalCloseButton =
  button !. "close" !+ "button" ! customAttribute "data-dismiss" "modal" $
  preEscapedText "&times;"

login :: Html
login = loginMessage ""

loginMessage :: Text -> Html
loginMessage m = baseTemplate "Login to Set Ladder" Nothing $ [shamlet|
  <div class="container">
    <div class="row">
      <div class="span12">
        <div class="jumboLogo largeCorners dropShadow">
        <h1 id="JumboHeader" class="alignCenter">the Stanford Set Ladder
    <div class="row">
      <div class="span6 offset3" id="LoginControls">
        $if (m == "loginfailed")
          <div class="alert alert-error">
            #{closeButton}
            <strong>Login failed!
            Incorrect username or password.
        $if (m == "registersuccess")
          <div class="alert alert-success">
            #{closeButton}
            <strong>Registration successful!
            You should now be able to log in.
        <form class="form-horizontal" action="/auth" method="POST">
          <div class="control-group">
            <label class="control-label" for="InputUsername">Username
            <div class="controls">
              <input type="text" id="InputUsername" name="username"
               placeholder="Username" autofocus="autofocus">
          <div class="control-group">
            <label class="control-label" for="InputPassword">Password
            <div class="controls">
              <input type="password" id="InputPassword" name="password"
               placeholder="Password">
          <div class="control-group">
            <div class="controls">
              <button type="submit" class="btn">Sign in
          <div class="control-group">
            <div class="controls">
              <small>Don't have an account?
              <a class="btn btn-small btn-inverse" href="/register">Register
  |]

register :: Html
register = registerMessage ""

registerMessage :: Text -> Html
registerMessage m = pageTemplate "Register for Set Ladder" scripts "" $ [shamlet|
  <div class="row">
    <div class="span12">
      <div class="page-header">
        <h2>Register for the Set Ladder
  <div class="row">
    <div class="span9">
      $if m == "invaliddata"
        <div class="alert alert-error"> 
          #{closeButton}
          <strong>Registration failed!
          Some data was invalid.
      <form class="form-horizontal" action="/doregister" method="POST">
        <div class="control-group" data-bind="css: {error: usernameError()}">
          <label class="control-label" for="InputUsername">Username
          <div class="controls">
            <input type="text" id="<InputUsername" class="value: enteredUsername"
             data-bind="value: enteredUsername"
             name="username" placeholder="Ex: lennartj"> 
            <span class="help-inline" data-bind="text: displayUsernameState">
        <div class="control-group">
          <label class="control-label" for="InputPassword1">Password
          <div class="controls">
            <input type="password" id="InputPassword1"
             data-bind="value: password1"
             name="password1" placeholder="Your password">
        <div class="control-group" data-bind="css: {error: passwordsDontMatch()}">
          <label class="control-label" for="InputPassword2">Password again
          <div class="controls">
            <input type="password" id="InputPassword2"
             data-bind="value: password2"
             name="password2" placeholder="Your password again">
            <span class="help-inline" data-bind="text: passwordsMatchText">
        <div class="control-group" data-bind="css: {error: emailIsInvalid()}">
          <label class="control-label" for="InputEmail">Email
          <div class="controls">
            <input type="text" id="InputEmail"
             data-bind="value: enteredEmail"
             name="email" placeholder="Ex: lennartj@stanford.edu">
            <span class="help-inline" != "text: displayEmailState">
            <span class="help-block">Please enter a valid email address for an account you have access to so we can send you a replacement if you forget your password. A stanford.edu domain email account is not required.
        <div class="control-group">
          <label class="control-label" for="InputRealName">Real name
          <div class="controls">
            <input type="text" id="InputRealName"
             name="realname" placeholder="Ex: Lennart Jansson">
            <span class="help-block">Optional, you can change this later from your user profile.
        <div class="control-group">
          <label class="control-label" for="InputLocation">Location
          <div class="controls">
            <input type="text" id="InputLocation"
             name="location" placeholder="Ex: Stanford, CA">
            <span class="help-block">Optional, you can change this later from your user profile.
        <div class="control-group">
          <div class="controls">
            <input type="submit" class="btn" data-bind="enable: submitButtonState(), buttonEnable: submitButtonState()" value="Register">
    <div class="span3">
      <p class="lead">Registration is free and easy and we probably won't misuse your personal information!
  |]
  where scripts = Just $ script ! A.src "/static/js/register.js" $ ""

notFound :: Html
notFound = baseTemplate "404" Nothing $ [shamlet|
  <div class="container">
    <div class="row">
      <div class="span12">
        <h1 class="alignCenter" style="line-height:1;margin-top:100px;font-size:100px">
          OH NOES :(
        <h1 class="alignCenter">404: page not found
  |]