{-# LANGUAGE TemplateHaskell #-}

module Types where

import Snap.Snaplet
import Snap.Snaplet.Session
import Data.Lens.Common
import Data.Lens.Template
import Snap.Snaplet.MongoDB
import qualified Control.Category as Cat
import Data.Text (Text)

data App = App {
  _sess  :: Snaplet SessionManager,
  _mongo :: Snaplet MongoDB
}
makeLens ''App
instance HasMongoDB App where
  getMongoDB = getL (snapletValue Cat.<<< mongo)

type AppHandler = Handler App App

-- View types
data PageName = Home | Login | Register | Profile | Friends | Other Text
              deriving (Show, Eq)
data NavbarEntry = NavbarEntry {
  pageName :: PageName,
  displayText :: Text,
  linkAddress :: Text
  } deriving (Show, Eq)