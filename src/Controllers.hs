{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import Snap.Core
import Control.Applicative
import Data.Maybe
import Snap.Snaplet
import Snap.Snaplet.Session
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Utf8
import Data.Text (Text)
--import qualified Data.Text as T
import Data.ByteString.Char8 (ByteString)
--import qualified Data.ByteString.Char8 as B
import Data.Text.Encoding
import Control.Monad.Trans
import Data.Aeson ((.=))
import qualified Data.Aeson as AE

import qualified Views as V
import Types
import Models

render :: Html -> AppHandler ()
render = writeLBS . renderHtml

requireAuth :: AppHandler () -> AppHandler ()
requireAuth handler = do
  maybeUsername <- getSessUsername
  case maybeUsername of
    Nothing -> redirect "/login"
    Just _ -> handler

requireNotAuth :: AppHandler () -> AppHandler ()
requireNotAuth handler = do
  maybeUsername <- getSessUsername
  case maybeUsername of
    Nothing -> handler
    Just _ -> redirect "/"

getSessUsername :: AppHandler (Maybe Text)
getSessUsername = with sess $ getFromSession "username"

getSessJustUsername :: AppHandler Text
getSessJustUsername = maybe "" id <$> getSessUsername

getTextParam :: ByteString -> AppHandler Text
getTextParam paramName = decodeUtf8 . (maybe "" id) <$> getParam paramName

-- CONTROLLERS

index :: AppHandler ()
index = requireAuth $ do
  username <- getSessJustUsername
  render $ V.index username

friends :: AppHandler ()
friends = requireAuth $ do
  username <- getSessJustUsername
  outFriends <- getOutFriendUsernames username
  inFriends <- getInFriendUsernames username
  render $ V.friends username outFriends inFriends

addFriend :: AppHandler ()
addFriend = requireAuth $ do
  username <- getSessJustUsername
  currentFriends <- getOutFriendUsernames username
  friendName <- getTextParam "friendName"
  friendNameExists <- not <$> isUsernameAvailable friendName
  if friendName == username
    then writeLBS "cannotaddself"
    else if friendName `elem` currentFriends
         then writeLBS "alreadyfriend"
         else if not friendNameExists
              then writeLBS "usernamenotfound"
              else do
                success <- addFriendship username friendName
                if success
                  then writeLBS "success"
                  else writeLBS "servererror"

unFriend :: AppHandler ()
unFriend = requireAuth $ do
  username <- getSessJustUsername
  currentFriends <- getOutFriendUsernames username
  friendName <- getTextParam "friendName"
  if not (friendName `elem` currentFriends)
    then writeLBS "notafriend"
    else do
      success <- doUnfriend username friendName
      if success
        then writeLBS "success"
        else writeLBS "servererror"

getFriends :: AppHandler ()
getFriends = do
  username <- getSessJustUsername
  friendNames <- getOutFriendUsernames username
  writeLBS $ AE.encode friendNames

profile :: AppHandler ()
profile = requireAuth $ do
  usernameParam <- getTextParam "username"
  if usernameParam == ""
    then profileSelf
    else profileUser usernameParam

{- Add different implementation here, needs to render a different view to
   have edit controls on page -}
profileSelf :: AppHandler ()
profileSelf = getSessJustUsername >>= profileUser

profileUser :: Text -> AppHandler ()
profileUser username = do
  self <- getSessJustUsername
  maybeUser <- lookupUserByName username
  case maybeUser of
    Left DBFail -> redirect "/servererror"
    Left ResultFail -> notFound
    Right user -> do
      let userProfileView =
            if self == username
            then V.userSelfProfile
            else V.userProfile
      render $ userProfileView (getUsername user) (getUserRealName user)
        (getUserLocation user)

login :: AppHandler ()
login = requireNotAuth $ do
  message <- getParam "message"
  case message of
    Just m -> render $ V.loginMessage (decodeUtf8 m)
    _ -> render V.login

auth :: AppHandler ()
auth = do
  username <- getTextParam "username"
  clearPassword <- getTextParam "password"
  authResult <- authenticateUser username clearPassword
  case authResult of
    Left DBFail -> redirect "/servererror"
    Left ResultFail -> redirect "/login/loginfailed"
    Right _ -> do
      withSession sess $ with sess $ setInSession "username" username
      redirect "/"

logout :: AppHandler ()
logout = do
  withSession sess $ with sess $ deleteFromSession "username"
  redirect "/login"

register :: AppHandler ()
register = do
  message <- getParam "message"
  case message of
    Just m -> render $ V.registerMessage (decodeUtf8 m)
    _ -> render V.register

doRegister :: AppHandler ()
doRegister = do
  username <- getTextParam "username"
  password1 <- getTextParam "password1"
  password2 <- getTextParam "password2"
  email <- getTextParam "email"
  realname <- getTextParam "realname"
  location <- getTextParam "location"
  userDataValid <- isUserDataValid username password1 password2 email
  if userDataValid
    then do
      commitedUser <-
        createAndCommitUser username password1 email realname location
      case commitedUser of
        Nothing -> redirect "/servererror"
        Just _ -> redirect "/login/registersuccess"
    else redirect "/register/invaliddata"

usernameAvailable :: AppHandler ()
usernameAvailable = do
  maybeUsername <- getParam "username"
  available <- case maybeUsername of
    Nothing -> return False
    Just username -> isUsernameAvailable $ decodeUtf8 username
  let jsonObj = AE.object ["usernameAvailable" .= available]
  writeLBS $ AE.encode jsonObj

usernameSearch :: AppHandler ()
usernameSearch = do
  maybeUsername <- getParam "username"
  matchingNames <- case maybeUsername of
    Nothing -> return []
    Just username -> getMatchingNames $ decodeUtf8 username
  writeLBS $ AE.encode matchingNames


notFound :: AppHandler ()
notFound = do
  getResponse >>= (putResponse . setResponseCode 404)
  render V.notFound