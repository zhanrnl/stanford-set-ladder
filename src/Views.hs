{-# LANGUAGE OverloadedStrings #-}

module Views where

import Prelude hiding (head, id, div, span)
import qualified Prelude as P
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Internal (Attributable, textValue)
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid ((<>))
import Control.Monad
import Data.Time
import System.Locale (defaultTimeLocale)

import Types
import Ratings

navbarEntries :: [NavbarEntry]
navbarEntries = [
  NavbarEntry Home "Home" "/",
  NavbarEntry Profile "Your profile" "/profile",
  NavbarEntry Friends "Friends" "/friends",
  NavbarHeader "Play SET!",
  NavbarEntry ReportOffline "Report offline 1v1 game" "/reportoffline",
  NavbarEntry ViewLadder "View 1v1 ladder" "/ladder"
  ]

maybeWhen :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
maybeWhen maybeA f =
  case maybeA of
    Nothing -> return ()
    Just a' -> f a'

stylesheetLink :: Text -> Html
stylesheetLink href = link ! A.rel "stylesheet" ! (A.href $ toValue href)

empty :: Html
empty = return ()

(!.) :: Attributable h => h -> AttributeValue -> h
tag !. val = tag ! A.class_ val
(!#) :: Attributable h => h -> AttributeValue -> h
tag !# val = tag ! A.id val
(!+) :: Attributable h => h -> AttributeValue -> h
tag !+ val = tag ! A.type_ val
(!=) :: Attributable h => h -> AttributeValue -> h
tag != val = tag ! customAttribute "data-bind" val

baseTemplate :: Text -> Maybe Html -> Html -> Html
baseTemplate titleText maybeHeadHtml contentHtml = docTypeHtml $ do
  head $ do
    title $ toHtml titleText
    stylesheetLink "/static/css/bootstrap.min.css"
    stylesheetLink "/static/css/darkstrap.css"
    stylesheetLink "/static/css/jquery-ui-1.9.0.custom.min.css"
    stylesheetLink "/static/font/fontface.css"
    stylesheetLink "/static/css/setladder.css"
    script ! A.src "/static/js/json2.js" $ empty
    script ! A.src "/static/js/jquery-1.8.1.min.js" $ empty
    script ! A.src "/static/js/jquery-ui-1.9.0.custom.min.js" $ empty
    script ! A.src "/static/js/bootstrap.min.js" $ empty
    script ! A.src "/static/js/knockout-2.1.0.js" $ empty
    maybeWhen maybeHeadHtml (>> return ())
  body $ contentHtml

pageTemplate :: Text -> Maybe Html -> Text -> Html -> Html
pageTemplate titleText maybeHeadHtml username contentHtml =
  baseTemplate titleText maybeHeadHtml $ div ! A.class_ "container" $ do
    div !. "row header" $ do
      div !. "span9" $ do
        div !. "headerLogo smallCorners dropShadow pull-left" $ empty
        h1 !. "logoText" $ "the Stanford Set Ladder"
      when (username /= "") $ do
        div !. "span3 alignRight userBox" $ do
          p $ do
            "Logged in as: "
            strong $ toHtml username
          div !. "btn-group" $ do
            --a !. "btn btn-small" $ "Edit profile" -- moved to navbar
            a !. "btn btn-small btn-inverse" ! A.href "/logout" $ "Log out"
    div !. "row" $
      div !. "span12" $ contentHtml

navHtml :: PageName -> Html
navHtml page = do
  ul !. "nav nav-pills nav-stacked" $ do
    forM_ navbarEntries $ \navEntry -> do
      case navEntry of
        NavbarHeader text -> do
          li !. "nav-header" $ toHtml text
        _ -> do
          activeIfMatch navEntry li $
            a ! A.href (textValue $ linkAddress navEntry) $
            toHtml $ displayText navEntry
  where activeIfMatch navEntry tag =
          if (pageName navEntry == page)
          then tag !. "active subtleDropShadow"
          else tag

pageTemplateNav :: PageName -> Text -> Maybe Html -> Text -> Html -> Html
pageTemplateNav page titleText maybeHeadHtml username contentHtml =
  pageTemplate titleText maybeHeadHtml username $ do
    div !. "row" $ do
      div !. "span3" $ navHtml page
      div !. "span9" $ contentHtml

index :: Text -> Html
index username = pageTemplateNav Home "Set Ladder: Home" Nothing username $ do
  h1  !. "page-title" $ "Welcome!"
  p "The Stanford Set Ladder is a player ranking database and system for competitive SET (the card game). Currently you can play games in real life and report the results to the Set Ladder and your and your opponent's ratings will be updated."
  h3 "Exciting features on the way"
  p "Eventually, you will be able to play games online in realtime against opponents, with an automatic matchmaking system. You will also be able play the SET Daily Puzzle and play through a full deck solitaire style to compete for the fastest time."

preEscapedText :: Text -> Html
preEscapedText = preEscapedToHtml

reportGame :: Text -> Text -> Html
reportGame username message = pageTemplateNav ReportOffline "Set Ladder: Report offline game" scripts username $ do
  h1 !. "page-title" $ "Report an Offline 1v1 Game"
  p "Report a game played offline (not on this site) between you and someone who has added you as a friend."
  when (message == "invalid") $ do
    div !. "alert alert-error" $ do
      closeButton
      strong "Invalid data sent to server! " >> "Perhaps the user you entered has not added you as a friend?"
  form !. "form-horizontal" ! A.action "/doreport" ! A.method "POST" $ do
    h3 "Players"
    div !. "control-group" $ do
      toHtml username
      " vs. "
      input !+ "text" !# "OpponentInput" ! A.placeholder "Opponent username" ! A.style "width: 160px"
        != "value: opponent, valueUpdate: 'afterkeydown'" ! A.name "opponentname"
    h3 "Result"
    div !. "control-group" != "css: {error: (isNaN(ownScoreValid()) && ownScore().length > 0) || (!numbersValid() && ownScore().length > 0 && opponentScore().length > 0)}" $ do
      label !. "control-label" ! A.style "text-align: left" $ do
        toHtml username
        " got "
      input !+ "text" !# "YourScore" ! A.placeholder "Your score" ! A.style "width: 120px"
        != "value: ownScore, valueUpdate: 'afterkeydown'" ! A.name "ownscore"
      " sets"
      span !. "help-block" != "text: ownScoreErrorText, visible: ownScoreErrorText().length > 0" $ empty
    div !. "control-group" != "css: {error: (isNaN(opponentScoreValid()) && opponentScore().length > 0) || (!numbersValid() && ownScore().length > 0 && opponentScore().length > 0)}" $ do
      label !. "control-label" ! A.style "text-align: left" $ do
        span != "text: opponent() == '' ? '[opponent]' : opponent()" $ empty
        " got "
      input !+ "text" !# "OpponentScore" ! A.placeholder "Opponent's score" ! A.style "width: 120px"
        != "value: opponentScore, valueUpdate: 'afterkeydown'" ! A.name "opponentscore"
      " sets"
      span !. "help-block" != "text: opponentScoreErrorText, visible: opponentScoreErrorText().length > 0" $ empty
      span !. "help-block" != "text: numbersValidText, visible: numbersValidText().length > 0" $ empty
    input !+ "submit" !. "btn" != "enable: submitButtonState(), buttonEnable: submitButtonState()" ! A.value "Report game"
  where scripts = Just $ do
          script ! A.src "/static/js/reportgame.js" $ empty

friends :: Text -> [Text] -> [Text] -> Html
friends username outFriends inFriends = pageTemplateNav Friends "Set Ladder: Friends" scripts username $ do
  h1  !. "page-title" $ "Friends"
  p "Adding friends makes it easier to keep track of their ratings and game records, and also allows them to report offline 1v1 games between the two of you. Simply type their usernames below to add them to your list of friends."
  form !. "form-inline" ! A.action "javascript:void(0)" $ do
    input !+ "text" !# "FriendNameInput"
      ! A.name "friendname" ! A.placeholder "Friend name"
      != "value: friendName, valueUpdate: 'afterkeydown'"
    button !# "AddFriendButton" !. "btn btn-success"
      != "click: addFriend" $
      "Add friend"
  div !# "ErrorContainer" $ empty
  h3 "Users you have added as a friend"
  div !. "modal hide fade" !# "UnfriendModal" ! A.tabindex "-1" ! customAttribute "role" "dialog" $ do
    div !. "modal-header" $ do
      modalCloseButton
      h3 "Unfriending"
    div !. "modal-body" $ do
      p $ do
        "Do you really want to unfriend "
        span != "text: unfriendUsername" $ empty
        "?"
    div !. "modal-footer" $ do
      button !. "btn" ! customAttribute "data-dismiss" "modal" $ "Cancel"
      button !. "btn btn-danger" != "click: doUnfriend" ! customAttribute "data-dismiss" "modal" $ "Unfriend"
  table !. "table table-condensed" != "visible: outFriends().length > 0" $ do
    thead $ do
      tr $ th $ "Username"
    tbody $ do
      preEscapedText "<!-- ko foreach: outFriends -->"
      tr $ do
        td != "text: $data" $ empty
        td $ do
          a != "attr: {href: '/profile/' + $data}" $ "View profile"
        td $ do
          a ! A.href "#UnfriendModal"
            != "click: $root.setUnfriendUsername($data)"
            ! customAttribute "data-toggle" "modal" $ "Unfriend"
      preEscapedText "<!-- /ko -->"
  p != "visible: outFriends().length == 0" $ "No friends added yet."
  h3 "Users who have added you as a friend"
  if (null inFriends)
    then do
    p "No one has added you as a friend yet."
    else do
    p "Since these users have given you permission, you can report offline 1v1 games between you and anyone on this list to update your rankings."
    table !. "table table-condensed" $ do
      thead $ do
        tr $ th $ "Username"
      tbody $ do
        forM_ inFriends $ \friendName -> do
          tr $ do
            td $ toHtml friendName
            td $ do
              a ! A.href (textValue $ "/profile/" <> friendName) $ "View profile"
  where scripts = Just $ do
          script $ toHtml $
            "var outFriendsInit = [" <>
            T.intercalate ", " (map (T.pack . show) outFriends) <> "];"
          script ! A.src "/static/js/friends.js" $ empty

viewLadder :: Text -> [(Text, Maybe Rating)] -> Html
viewLadder username userRatings = pageTemplateNav ViewLadder "Set Ladder: 1v1 Ladder" Nothing username $ do
  h1  !. "page-title" $ "The 1v1 Ladder"
  table !. "table table-condensed" $ do
    thead $ do
      tr $ do
        th "Rank"
        th "Username"
        th "Rating"
    tbody $ do
      forM_ (zip userRatings [1..]) $ \((name, ratingM), rank) -> do
        tr !. (if name == username then "highlight" else "") $ do
          td $ do
            when (isJust ratingM) $ toHtml (rank :: Int)
            return ()
          td $ toHtml name
          td $ case ratingM of
            Just rating -> toHtml rating
            Nothing -> span !. "muted" $ "(none)"
          td $ do
            a ! A.href (textValue $ "/profile/" <> name) $ "View profile"

userProfileNoTemplate :: Text -> Text -> Text -> Maybe Rating -> [GameRecordDisplay] -> Text -> Html
userProfileNoTemplate username realname location rating recentGames message = do
  h1 !. "page-title" $ toHtml $ "User profile of " <> username
  p $ do
    strong "Real name: "
    toHtmlNoneGiven realname
  p $ do
    strong "Location: "
    toHtmlNoneGiven location
  when (message == "reportsuccess") $ do
    div !. "alert alert-success" $ do
      closeButton
      strong "Offline game report successful!"
  div !. "row" $ do
    div !. "span3" $ do
      h3 "1v1 SET Rating"
      if isJust rating
        then div !. "rating" $ toHtml $ fromJust rating
        else toHtmlNoneGiven ""
    div !. "span6" $ do
      h3 "Recent 1v1 games"
      if null recentGames
        then toHtmlNoneGiven ""
        else table !. "table table-condensed" $ do
        thead $ do
          tr $ do
            th "Versus"
            th "Result"
            th "Rating"
            th "Date (GMT)"
        tbody $ do
          forM_ recentGames $ \gameRecord -> do
            tr $ do
              td $ toHtml $ grVersus gameRecord
              td $ do
                toResult (grOwnScore gameRecord) (grOpponentScore gameRecord)
                toHtml $ grOwnScore gameRecord
                "-"
                toHtml $ grOpponentScore gameRecord
              td $ do
                toHtml $ grRating gameRecord
              td $ do
                toHtml $ formatTime defaultTimeLocale "%-D %-R" (grTime gameRecord)
  where toResult ownScore opponentScore
          | ownScore > opponentScore = span !. "text-success" $ "Win "
          | opponentScore > ownScore = span !. "text-error" $ "Lose "
          | otherwise = "Tie "

userProfile :: Text -> Text -> Text -> Maybe Rating -> [GameRecordDisplay] -> Text -> Html
userProfile username realname location rating recentGames message = pageTemplateNav Other ("Set Ladder: Profile of " <> username) Nothing username $ userProfileNoTemplate username realname location rating recentGames message

userSelfProfile :: Text -> Text -> Text -> Maybe Rating -> [GameRecordDisplay] -> Text -> Html
userSelfProfile username realname location rating recentGames message = pageTemplateNav Profile "Set Ladder: Your profile" Nothing username $ userProfileNoTemplate username realname location rating recentGames message

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
loginMessage m = baseTemplate "Login to Set Ladder" Nothing $ div !. "container" $ do
  div !. "row" $ div !. "span12" $ do
    div !. "jumboLogo largeCorners dropShadow" $ empty
    h1 !# "JumboHeader" !. "alignCenter" $ "the Stanford Set Ladder"
  div !. "row" $ div !. "span6 offset3" !# "LoginControls" $ do
    when (m == "loginfailed") $ do
      div !. "alert alert-error" $ do
        closeButton
        strong "Login failed! " >> "Incorrect username or password."
    when (m == "registersuccess") $ do
      div !. "alert alert-success" $ do
        closeButton
        strong "Registration successful! " >> "You should now be able to log in."
    form !. "form-horizontal" ! A.action "/auth" ! A.method "POST" $ do
      div !. "control-group" $ do
        label !. "control-label" ! A.for "InputUsername" $ "Username"
        div !. "controls" $ do
          input !+ "text" !# "InputUsername" ! A.name "username"
            ! A.placeholder "Username" ! A.autofocus "autofocus"
      div !. "control-group" $ do
        label !. "control-label" ! A.for "InputPassword" $ "Password"
        div !. "controls" $ do
          input !+ "password" !# "InputPassword" ! A.name "password"
            ! A.placeholder "Password"
      div !. "control-group" $ do
        div !. "controls" $ do
          button !+ "submit" !. "btn" $ "Sign in"
      div !. "control-group" $ do
        div !. "controls" $ do
          small $ "Don't have an account? "
          a !. "btn btn-small btn-inverse" ! A.href "/register" $ "Register"

register :: Html
register = registerMessage ""

registerMessage :: Text -> Html
registerMessage m = pageTemplate "Register for Set Ladder" scripts "" $ do
  div !. "row" $ do
    div !. "span12" $ do
      div !. "page-header" $ do
        h2 "Register for the Set Ladder"
  div !. "row" $ do
    div !. "span9" $ do
      when (m == "invaliddata") $ do
        div !. "alert alert-error" $ do
          closeButton
          strong "Registration failed! " >> "Some data was invalid."
      form !. "form-horizontal" ! A.action "/doregister"
        ! A.method "POST" $ do
        div !. "control-group" != "css: {error: usernameError()}" $ do
          label !. "control-label" ! A.for "InputUsername" $ "Username"
          div !. "controls" $ do
            input !+ "text" !# "InputUsername"
              != "value: enteredUsername"
              ! A.name "username" ! A.placeholder "Ex: lennartj"
            " "
            span !. "help-inline" != "text: displayUsernameState" $ empty
        div !. "control-group" $ do
          label !. "control-label" ! A.for "InputPassword1" $ "Password"
          div !. "controls" $ do
            input !+ "password" !# "InputPassword1"
              != "value: password1"
              ! A.name "password1" ! A.placeholder "Your password"
        div !. "control-group" != "css: {error: passwordsDontMatch()}" $ do
          label !. "control-label" ! A.for "InputPassword2" $
            "Password again"
          div !. "controls" $ do
            input !+ "password" !# "InputPassword2"
              != "value: password2"
              ! A.name "password2" ! A.placeholder "Your password again"
            span !. "help-inline" != "text: passwordsMatchText" $ empty
        div !. "control-group" != "css: {error: emailIsInvalid()}" $ do
          label !. "control-label" ! A.for "InputEmail" $ "Email"
          div !. "controls" $ do
            input !+ "text" !# "InputEmail"
              != "value: enteredEmail"
              ! A.name "email" ! A.placeholder "Ex: lennartj@stanford.edu"
            span !. "help-inline" != "text: displayEmailState" $ empty
            span !. "help-block" $
              "Please enter a valid email address for an account you have access to so we can send you a replacement if you forget your password. A stanford.edu domain email account is not required."
        div !. "control-group" $ do
          label !. "control-label" ! A.for "InputRealName" $ "Real name"
          div !. "controls" $ do
            input !+ "text" !# "InputRealName"
              ! A.name "realname" ! A.placeholder "Ex: Lennart Jansson"
            span !. "help-block" $ "Optional, you can change this later from your user profile."
        div !. "control-group" $ do
          label !. "control-label" ! A.for "InputLocation" $ "Location"
          div !. "controls" $ do
            input !+ "text" !# "InputLocation"
              ! A.name "location" ! A.placeholder "Ex: Stanford, CA"
            span !. "help-block" $ "Optional, you can change this later from your user profile."
        div !. "control-group" $ do
          div !. "controls" $ do
            input !+ "submit" !. "btn" != "enable: submitButtonState(), buttonEnable: submitButtonState()" ! A.value "Register"
    div !. "span3" $ do
      p !. "lead" $ "Registration is free and easy and we probably won't misuse your personal information!"
  where scripts = Just $ do
          script ! A.src "/static/js/register.js" $ empty


notFound :: Html
notFound = baseTemplate "404" Nothing $ div !. "container" $ do
  div !. "row" $ div !. "span12" $ do
    h1 !. "alignCenter" ! A.style "line-height:1;margin-top:100px;font-size:100px" $ "OH NOES :("
    h1 !. "alignCenter" $ "404: page not found"