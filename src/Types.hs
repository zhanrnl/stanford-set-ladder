{-# LANGUAGE TemplateHaskell #-}

module Types where

import Snap.Snaplet
import Snap.Snaplet.Session
import Data.Lens.Common
import Data.Lens.Template
import Snap.Snaplet.MongoDB
import qualified Control.Category as Cat

data App = App {
  _sess  :: Snaplet SessionManager,
  _mongo :: Snaplet MongoDB
}
makeLens ''App
instance HasMongoDB App where
  getMongoDB = getL (snapletValue Cat.<<< mongo)

type AppHandler = Handler App App