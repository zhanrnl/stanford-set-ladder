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

data DBError = DBFail | ResultFail deriving (Eq, Show)
type DBEither a = Either DBError a

-- For ratings
data GameType = Offline | Other
              deriving (Show, Eq)

-- View types
data PageName = Home | Profile | Friends
              | ReportOffline
              | Other
              deriving (Show, Eq)
data NavbarEntry = NavbarEntry {pageName :: PageName,
                                displayText :: Text,
                                linkAddress :: Text}
                 | NavbarHeader Text
                 deriving (Show, Eq)