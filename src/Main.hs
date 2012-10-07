{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Snap.Snaplet
import Snap.Snaplet.Session.Backends.CookieSession

import Snap.Snaplet.MongoDB
import Database.MongoDB

import Types
import qualified Controllers as C

appInit :: SnapletInit App App
appInit = makeSnaplet "setladder" "" Nothing $ do
  s <- nestSnaplet "sess" sess $ initCookieSessionManager "site-key.txt" "sess" Nothing
  m <- nestSnaplet "mongo" mongo $ mongoDBInit 10 (host "127.0.0.1") "setladder"
  addRoutesFull [("$", C.index),
                 ("index$", C.index),
                 ("login$", C.login),
                 ("login/:message", C.login),
                 ("auth$", method POST C.auth),
                 ("logout$", C.logout),
                 ("register$", C.register),
                 ("register/:message", C.register),
                 ("test$", C.test),
                 ("doregister$", method POST C.doRegister),
                 ("usernameavailable/:username$", C.usernameAvailable),
                 ("static", serveDirectory "static")]
  return $ App s m

-- | Processes routes before sending them to addRoutes to make routes that
-- end with a $ not accept longer paths (must match exactly)
addRoutesFull :: [(ByteString, AppHandler ())] -> Initializer App App ()
addRoutesFull = addRoutes . map modifyRoute
  where modifyRoute (bs, hand)
          | "$" `B.isSuffixOf` bs = (B.init bs, ifTop hand)
          | otherwise             = (bs, hand)

main :: IO ()
main = serveSnaplet defaultConfig appInit