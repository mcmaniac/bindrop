{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Session where

import Control.Lens.TH ( makeLenses )
import Data.Data ( Data, Typeable )
import Data.SafeCopy ( base, deriveSafeCopy )
import Happstack.Server.ClientSession

import Bindrop.State.Users

data SessionData = SessionData
  { _sessionUser :: Maybe User
  } deriving (Eq, Ord, Show, Data, Typeable)

$(makeLenses ''SessionData)
$(deriveSafeCopy 0 'base ''SessionData)

instance ClientSession SessionData where
  emptySession = SessionData { _sessionUser = Nothing }

