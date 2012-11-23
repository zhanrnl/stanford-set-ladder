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
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding
import Control.Monad.Trans
import Data.Aeson ((.=))
import qualified Data.Aeson as AE
import Data.Monoid ((<>))
import Data.Time
import qualified Data.Map as M

import qualified Views as V
import Types
import Models
import SetHelpers


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

getTextParam :: B.ByteString -> AppHandler Text
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

profileSelf :: Text -> AppHandler ()
profileSelf message = getSessJustUsername >>= profileUser message

profileUser :: Text -> Text -> AppHandler ()
profileUser message username = do
  self <- getSessJustUsername
  maybeUser <- lookupUserByName username
  ratingE <- get1v1Rating False username
  recentGames <- getRecentGames username
  recentPuzzleTimes <- getRecentPuzzleTimes username
  case maybeUser of
    Left DBFail -> redirect "/servererror"
    Left ResultFail -> notFound
    Right user -> do
      let rating = case ratingE of
            Left _ -> Nothing
            Right r -> Just r 
      render $ V.userProfile self (getUsername user) (getUserRealName user)
        (getUserLocation user) rating recentGames recentPuzzleTimes message

changeRealName :: AppHandler ()
changeRealName = requireAuth $ do
  self <- getSessJustUsername
  newName <- getTextParam "newname"
  success <- setRealName self newName
  if success
    then redirect "/profile/m/namechangesuccess"
    else redirect "/profile/m/namechangefail"
         
changeLocation :: AppHandler ()
changeLocation = requireAuth $ do
  self <- getSessJustUsername
  newLocation <- getTextParam "newlocation"
  success <- setLocation self newLocation
  if success
    then redirect "/profile/m/locationchangesuccess"
    else redirect "/profile/m/locationchangefail"

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

practicePuzzle :: AppHandler ()
practicePuzzle = requireAuth $ do
  self <- getSessJustUsername
  render $ V.practicePuzzle self

playPracticePuzzle :: AppHandler ()
playPracticePuzzle = requireAuth $ do
  self <- getSessJustUsername
  puzzle <- liftIO $ makePuzzle
  let cardNames = map (T.pack . show . fst) puzzle
      cardJSON = decodeUtf8 $ toStrict $
                 AE.encode (zip cardNames $ map snd puzzle)
  render $ V.playPracticePuzzle self cardJSON
  where toStrict = B.concat . BL.toChunks
        
dailyPuzzle :: AppHandler ()
dailyPuzzle = requireAuth $ do
  self <- getSessJustUsername
  maybeHasStarted <- hasUserStartedPuzzle self
  case maybeHasStarted of
    Left _ -> redirect "/servererror"
    Right hasStarted -> do
      render $ V.dailyPuzzle self hasStarted

playDailyPuzzle :: AppHandler ()
playDailyPuzzle = requireAuth $ do
  self <- getSessJustUsername
  maybeHasStarted <- hasUserStartedPuzzle self
  case maybeHasStarted of
    Left _ -> redirect "/servererror"
    Right hasStarted ->
      if hasStarted
      then redirect "/dailypuzzle"
      else do
        maybePuzzle <- getDailyPuzzle  
        case maybePuzzle of
          Left _ -> redirect "/servererror"
          Right puzzle -> do
            let cardNames = map (T.pack . show . fst) puzzle
                cardJSON = decodeUtf8 $ toStrict $
                           AE.encode (zip cardNames $ map snd puzzle)
            render $ V.playDailyPuzzle self cardJSON
  where toStrict = B.concat . BL.toChunks

puzzleLadder :: AppHandler ()
puzzleLadder = requireAuth $ do
  self <- getSessJustUsername
  day <- today
  offset <- getTextParam "offset"
  let offsetDay =
        if offset == ""
        then day
        else UTCTime (addDays (negate $ read $ T.unpack offset) $ utctDay day) (utctDayTime day)
  ladder <- getPuzzleLadder offsetDay
  if offset == ""
    then render $ V.viewPuzzleLadder self ladder offsetDay day 0
    else render $ V.viewPuzzleLadder self ladder offsetDay day (read $ T.unpack offset)

puzzleStarted :: AppHandler ()
puzzleStarted = requireAuth $ do
  self <- getSessJustUsername
  setUserStartedPuzzle self
  return ()

puzzleCompleted :: AppHandler ()
puzzleCompleted = requireAuth $ do
  self <- getSessJustUsername
  timeText <- getTextParam "time"
  if (timeText == "")
    then puzzleDNF
    else do
    let time :: Integer
        time = read (T.unpack timeText)
    setUserCompletedPuzzle self time
    return ()

puzzleDNF :: AppHandler ()
puzzleDNF = requireAuth $ do
  self <- getSessJustUsername
  setUserDNFPuzzle self
  return ()

hasStartedPuzzle :: AppHandler ()
hasStartedPuzzle = requireAuth $ do
  self <- getSessJustUsername
  hasStarted <- hasUserStartedPuzzle' self
  writeLBS $ AE.encode hasStarted

        
{-
test :: AppHandler ()
test = do
  puzzle <- getDailyPuzzle
  liftIO $ print puzzle
  return ()
-}

notFound :: AppHandler ()
notFound = do
  getResponse >>= (putResponse . setResponseCode 404)
  render V.notFound