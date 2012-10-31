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

import Types

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

addFriendship :: Text -> Text -> AppHandler Bool
addFriendship username friendName = do
  maybeResult <- maybeWithDB $ M.insert "friends" ["username" =: username, "friendswith" =: friendName]
  return $ isJust maybeResult

doUnfriend :: Text -> Text -> AppHandler Bool
doUnfriend username friendName = do
  maybeResult <- maybeWithDB $ 
    M.delete $ M.select ["username" =: username, "friendswith" =: friendName] "friends"
  return $ isJust maybeResult

