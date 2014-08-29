{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Bindrop.State.Users where

import Control.Lens.TH      ( makeLenses )
import Data.Acid
import Data.Data            ( Data, Typeable )
import Data.IxSet           ( Indexable(..), (@=), (@<), getOne
                            , inferIxSet, noCalcs, toDescList)
import qualified Data.IxSet as IxSet
import Data.SafeCopy        ( base, deriveSafeCopy )
import qualified Data.ByteString as B

newtype UserID = UserID {unUserID :: Integer}
  deriving (Eq, Ord, Show, Data, Enum, Typeable, Num)

$(deriveSafeCopy 0 'base ''UserID)

data User = User { _userID  :: UserID
                 , _uName   :: String --user name
                 , _uEmail  :: String --user email
                 , _uPass   :: B.ByteString --user password
                 , _uPhrase :: String --user passphrase for account recovery
                 , _uAnswer :: B.ByteString --answer to passphrase
                 , _count   :: Int --upload count for stats
                 } deriving (Eq, Ord, Show, Data, Typeable)

$(makeLenses ''User)

$(deriveSafeCopy 0 'base ''User)

inferIxSet "UserDB" ''User 'noCalcs
  [ ''UserID
  , ''String
  , ''String
  , ''B.ByteString
  , ''String
  , ''B.ByteString
  , ''Int
  ]

data Users =
  Users { _nextUserID :: UserID
        , _users      :: UserDB }
          deriving (Data, Typeable)

$(makeLenses ''Users)

$(deriveSafeCopy 0 'base ''Users)

initialUsersState :: Users
initialUsersState = Users (UserID 1) empty

