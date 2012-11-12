{-# LANGUAGE OverloadedStrings #-}

module Models where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Snap.Snaplet.MongoDB
import Database.MongoDB ((=:), (!?), Document)
import qualified Database.MongoDB as M hiding (index)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
--import Data.ByteString as WB
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Monoid
import Data.Maybe
import System.Random
import Data.List
import Data.Char
import Numeric (showHex)
import qualified Text.Email.Validate as Email
import Data.Time

import Types
import Ratings

-- USER (user authentication, user data validation, user lookup) --

data User = User {
  getUsername :: Text,
  getUserPassHash :: ByteString,
  getUserPassSalt :: ByteString,
  getUserEmail :: Text,
  getUserRealName :: Text,
  getUserLocation :: Text
} deriving (Eq, Show)

hashPass :: Text -> ByteString -> ByteString
hashPass clear salt = SHA1.hash $ (encodeUtf8 clear) <> salt

makeSalt :: MonadIO m => m ByteString
makeSalt = liftIO $ B.pack <$> replicateM 16 (randomRIO ('a', 'z'))

prettyPrint :: ByteString -> Text
prettyPrint = T.pack . concatMap (\c -> showHex (ord c) "") . B.unpack

unPrettyPrint :: Text -> ByteString
unPrettyPrint = B.pack . map (chr . read . T.unpack . ("0x" <>)) .
                takeWhile (not . T.null) . unfoldr (Just . T.splitAt 2)

createUser :: Text -> Text -> Text -> Text -> Text -> AppHandler User
createUser username clear email realName location = do
  salt <- makeSalt
  let passHash = hashPass clear salt
  return $ User username passHash salt
    email realName location

commitUser :: User -> AppHandler (Maybe M.Value)
commitUser (User username passHash salt email realName location) = do
  maybeWithDB $ M.insert "users" [
    "username" =: username, "passHash" =: B.unpack passHash,
    "salt" =: B.unpack salt, "email" =: email,
    "realname" =: realName, "location" =: location]
  where defaultRating = (1500 :: Int)

createAndCommitUser :: Text -> Text -> Text -> Text -> Text ->
                       AppHandler (Maybe M.Value)
createAndCommitUser username clear email realName location =
  commitUser =<< createUser username clear email realName location

userDocToUser :: Document -> AppHandler User
userDocToUser doc = do
  let username = maybe "" id $ doc !? "username"
      passHash = maybe "" B.pack $ doc !? "passHash"
      salt = maybe "" B.pack $ doc !? "salt"
      email = maybe "" id $ doc !? "email"
      realname = maybe "" id $ doc !? "realname"
      location = maybe "" id $ doc !? "location"
  return $ User username passHash salt email realname location

lookupUserByName :: Text -> AppHandler (DBEither User)
lookupUserByName username = do
  maybeResult <- maybeWithDB $ M.findOne $
                 M.select ["username" =: username] "users"
  case maybeResult of
    Nothing -> return $ Left DBFail
    Just Nothing -> return $ Left ResultFail
    Just (Just doc) -> do
      userObj <- userDocToUser doc
      return $ Right userObj

authenticateUser :: Text -> Text -> AppHandler (DBEither ())
authenticateUser username clear = do
  eitherUser <- lookupUserByName username
  let eitherMatch = do
        user <- eitherUser
        let salt = getUserPassSalt user
            refPass = getUserPassHash user
            checkPass = hashPass clear salt
        return (prettyPrint refPass == prettyPrint checkPass)
  return $ case eitherMatch of
    Left err -> Left err
    Right False -> Left ResultFail
    Right True -> Right ()

isUsernameAvailable :: Text -> AppHandler Bool
isUsernameAvailable username = do
  eitherUser <- lookupUserByName username
  case eitherUser of
    Left DBFail -> return False -- if db error, shouldn't allow registration
    Right _ -> return False     -- if nonnull result set, not available
    Left ResultFail -> return True

getMatchingNames :: Text -> AppHandler [Text]
getMatchingNames username = do
  maybeResult <- maybeWithDB $ do
    cursor <- M.find (M.select ["username" =: ["$regex" =: (T.unpack username), "$options" =: ("i"::String)]] "users"){M.project = ["username" =: (1::Int)]}
    M.rest cursor
  case maybeResult of
    Nothing -> return []
    Just names -> return $ map (T.pack . fromJust . (!? "username")) names

getMatchingInfriends :: Text -> Text -> AppHandler [Text]
getMatchingInfriends username query = do
  maybeResult <- maybeWithDB $ do
    cursor <- M.find (M.select ["username" =: ["$regex" =: (T.unpack query), "$options" =: ("i"::String)],
                                "friendswith" =: username] "friends"){M.project = ["username" =: (1::Int)]}
    M.rest cursor
  case maybeResult of
    Nothing -> return []
    Just names -> return $ map (T.pack . fromJust . (!? "username")) names

arePasswordsGood :: Text -> Text -> Bool
arePasswordsGood pass1 pass2 =
  (not $ T.null pass1) && (not $ T.null pass2) && (pass1 == pass2)

isUserDataValid :: Text -> Text -> Text -> Text -> AppHandler Bool
isUserDataValid username password1 password2 email = do
  usernameGood <- (&& not (T.null username)) <$> isUsernameAvailable username
  let passwordsGood = arePasswordsGood password1 password2
      emailGood = Email.isValid $ T.unpack email
  return (usernameGood && passwordsGood && emailGood)


-- FRIENDS

getOutFriendUsernames :: Text -> AppHandler [Text]
getOutFriendUsernames username = do
  maybeResult <- maybeWithDB $ do
    cursor <- M.find (M.select ["username" =: username] "friends"){M.project = ["friendswith" =: (1::Int)]}
    M.rest cursor
  case maybeResult of
    Nothing -> return []
    Just names -> return $ map (T.pack . fromJust . (!? "friendswith")) names

getInFriendUsernames :: Text -> AppHandler [Text]
getInFriendUsernames username = do
  maybeResult <- maybeWithDB $ do
    cursor <- M.find (M.select ["friendswith" =: username] "friends"){M.project = ["username" =: (1::Int)]}
    M.rest cursor
  case maybeResult of
    Nothing -> return []
    Just names -> return $ map (T.pack . fromJust . (!? "username")) names

isFriendsWith :: Text -> Text -> AppHandler Bool
isFriendsWith username friendName = do
  maybeResult <- maybeWithDB $ 
    M.count $ M.select ["username" =: username, "friendwith" =: friendName] "friends"
  liftIO $ do
    putStrLn "hello"
    print username
    print friendName
    print maybeResult
  return $ maybe False (>= 1) maybeResult

addFriendship :: Text -> Text -> AppHandler Bool
addFriendship username friendName = do
  maybeResult <- maybeWithDB $ M.insert "friends" ["username" =: username, "friendswith" =: friendName]
  return $ isJust maybeResult

doUnfriend :: Text -> Text -> AppHandler Bool
doUnfriend username friendName = do
  maybeResult <- maybeWithDB $ 
    M.delete $ M.select ["username" =: username, "friendswith" =: friendName] "friends"
  return $ isJust maybeResult


-- 1V1 RATINGS

-- | Put a game record in the db.
record1v1Game :: GameType -> (Text, Text) -> (Score, Score) -> (Rating, Rating) -> AppHandler Bool
record1v1Game gametype player score newRating = do
  time <- liftIO $ getCurrentTime
  maybeResult <- maybeWithDB $ M.insert "gamerecords"
                 ["playerA" =: (fst player), "playerB" =: (snd player),
                  "scoreA" =: (fst score), "scoreB" =: (snd score),
                  "newRatingA" =: (fst newRating),
                  "newRatingB" =: (snd newRating),
                  "gametype" =: show gametype, "time" =: time]
  return $ isJust maybeResult

getRecentGames :: Text -> AppHandler [GameRecordDisplay]
getRecentGames username = do
  maybeResult <- maybeWithDB $ do
    cursor <- M.find (M.select ["$or" =: [["playerA" =: username], ["playerB" =: username]]] "gamerecords")
      {M.limit = 10, M.sort = ["time" =: (-1::Int)]}
    M.rest cursor
  if isJust maybeResult
    then do
    let result = fromJust maybeResult
    return $ map makeGameRecordDisplay result
    else return []
  where makeGameRecordDisplay doc =
          if (fromJust (doc !? "playerA") == username)
          then GameRecordDisplay Offline
               (fromJust $ doc !? "playerB")
               (fromJust $ doc !? "scoreA")
               (fromJust $ doc !? "scoreB")
               (fromJust $ doc !? "newRatingA")
               (fromJust $ doc !? "time")
          else GameRecordDisplay Offline
               (fromJust $ doc !? "playerA")
               (fromJust $ doc !? "scoreB")
               (fromJust $ doc !? "scoreA")
               (fromJust $ doc !? "newRatingB")
               (fromJust $ doc !? "time")

get1v1Rating :: Bool -> Text -> AppHandler (DBEither Rating)
get1v1Rating useDefault username = do
  maybeResult <- maybeWithDB $ M.findOne
                 (M.select ["username" =: username] "users"){M.project = ["1v1rating" =: (1::Int)]}
  case maybeResult of
    Nothing -> return $ Left DBFail
    Just Nothing -> return $ Left ResultFail
    Just (Just doc) -> do
      let maybeRating = doc !? "1v1rating"
      if useDefault
        then return $ Right $ maybe defaultRating id maybeRating
        else if isJust maybeRating
             then return $ Right $ fromJust maybeRating
             else return $ Left ResultFail
  where defaultRating = 1500

get1v1RatingPlayers :: (Text, Text) -> AppHandler (DBEither (Rating, Rating))
get1v1RatingPlayers player = do
  ratingAE <- get1v1Rating True (fst player)
  case ratingAE of
    Left e -> return $ Left e
    Right ratingA -> do
      ratingBE <- get1v1Rating True (snd player)
      case ratingBE of
        Left e -> return $ Left e
        Right ratingB -> do
          return $ return (ratingA, ratingB)

set1v1Rating :: Text -> Rating -> AppHandler Bool
set1v1Rating username rating = do
  let modifier = ["$set" =: ["1v1rating" =: rating]]
  maybeResult <- maybeWithDB $ M.modify (M.select ["username" =: username] "users") modifier
  return $ isJust maybeResult

record1v1AndUpdate :: GameType -> (Text, Text) -> (Score, Score) -> AppHandler Bool
record1v1AndUpdate gametype player score = do
  ratingE <- get1v1RatingPlayers player
  case ratingE of
    Left _ -> return False
    Right rating -> do
      let newRating = updateRatings score rating
      successR <- record1v1Game gametype player score newRating
      successA <- set1v1Rating (fst player) (fst newRating)
      successB <- set1v1Rating (snd player) (snd newRating)
      return $ successR && successA && successB

getLadder :: AppHandler [(Text, Maybe Rating)]
getLadder = do
  maybeResult <- maybeWithDB $ do
    cursor <- M.find (M.select [] "users"){M.sort = ["1v1rating" =: (-1::Int)]}
    M.rest cursor
  case maybeResult of
    Nothing -> return []
    Just result -> return $ map (\doc -> (fromJust $ doc !? "username", doc !? "1v1rating")) result