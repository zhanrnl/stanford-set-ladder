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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
  self <- getSessJustUsername
  render $ V.index self

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

infriendSearch :: AppHandler ()
infriendSearch = requireAuth $ do
  self <- getSessJustUsername
  query <- getTextParam "query"
  inFriends <- getMatchingInfriends self query
  writeLBS $ AE.encode inFriends

friends :: AppHandler ()
friends = requireAuth $ do
  self <- getSessJustUsername
  outFriends <- getOutFriendUsernames self
  inFriends <- getInFriendUsernames self
  render $ V.friends self outFriends inFriends

addFriend :: AppHandler ()
addFriend = requireAuth $ do
  self <- getSessJustUsername
  currentFriends <- getOutFriendUsernames self
  friendName <- getTextParam "friendName"
  friendNameExists <- not <$> isUsernameAvailable friendName
  if friendName == self
    then writeLBS "cannotaddself"
    else if friendName `elem` currentFriends
         then writeLBS "alreadyfriend"
         else if not friendNameExists
              then writeLBS "usernamenotfound"
              else do
                success <- addFriendship self friendName
                if success
                  then writeLBS "success"
                  else writeLBS "servererror"

unFriend :: AppHandler ()
unFriend = requireAuth $ do
  self <- getSessJustUsername
  currentFriends <- getOutFriendUsernames self
  friendName <- getTextParam "friendName"
  if not (friendName `elem` currentFriends)
    then writeLBS "notafriend"
    else do
      success <- doUnfriend self friendName
      if success
        then writeLBS "success"
        else writeLBS "servererror"

getFriends :: AppHandler ()
getFriends = do
  self <- getSessJustUsername
  friendNames <- getOutFriendUsernames self
  writeLBS $ AE.encode friendNames

profile :: AppHandler ()
profile = requireAuth $ do
  usernameParam <- getTextParam "username"
  message <- getTextParam "message"
  if usernameParam == ""
    then profileSelf message
    else profileUser "" usernameParam

-- TODO: Add different implementation here, needs to render a different view to
-- have edit controls on page
profileSelf :: Text -> AppHandler ()
profileSelf message = getSessJustUsername >>= profileUser message

profileUser :: Text -> Text -> AppHandler ()
profileUser message username = do
  self <- getSessJustUsername
  maybeUser <- lookupUserByName username
  ratingE <- get1v1Rating False username
  recentGames <- getRecentGames username
  case maybeUser of
    Left DBFail -> redirect "/servererror"
    Left ResultFail -> notFound
    Right user -> do
      let rating = case ratingE of
            Left _ -> Nothing
            Right r -> Just r 
      render $ V.userProfile self (getUsername user) (getUserRealName user)
        (getUserLocation user) rating recentGames message

reportGame :: AppHandler ()
reportGame = requireAuth $ do
  self <- getSessJustUsername
  message <- getTextParam "message"
  render $ V.reportGame self message

doReport :: AppHandler ()
doReport = requireAuth $ do
  self <- getSessJustUsername
  opponent <- getTextParam "opponentname"
  ownScore <- ((read :: String -> Int) . T.unpack) <$> getTextParam "ownscore"
  opponentScore <- ((read :: String -> Int) . T.unpack) <$> getTextParam "opponentscore"
  inFriends <- getInFriendUsernames self
  let validOpponent = opponent `elem` inFriends
  if (validOpponent && (scoresValid ownScore opponentScore))
    then do success <- record1v1AndUpdate Offline (self, opponent) (ownScore, opponentScore)
            if success
              then redirect "/profile/m/reportsuccess"
              else redirect "/servererror"
    else redirect "/reportoffline/invalid"
  where scoreValid score = score >= 0 && score <= 27
        scoresValid scoreA scoreB = ((scoreA + scoreB) <= 27) && scoreValid scoreB && scoreValid scoreB

viewLadder :: AppHandler ()
viewLadder = requireAuth $  do
  self <- getSessJustUsername
  ladder <- getLadder
  render $ V.viewLadder self ladder
        
{-
test :: AppHandler ()
test = do
  success <- record1v1AndUpdate Offline ("lennartj", "arun_bassoon") (10, 17)
  writeText $ T.pack $ show success
-}


notFound :: AppHandler ()
notFound = do
  getResponse >>= (putResponse . setResponseCode 404)
  render V.notFound