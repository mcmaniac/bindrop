{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  TemplateHaskell, TypeFamilies,
  OverloadedStrings, RecordWildCards #-}

module Users where

import Control.Monad.State  ( get, put )
import Control.Monad.Reader ( ask )
import Control.Lens         ( (&), (^.), (%~) )
import Control.Lens.TH      ( makeLenses )
import Data.Acid
import Data.Data            ( Data, Typeable )
import Data.IxSet           ( Indexable(..), (@=), (@<), getOne
                            , inferIxSet, noCalcs, toDescList)
import qualified Data.IxSet as IxSet
import Data.SafeCopy        ( base, deriveSafeCopy )

import Crypto.Scrypt

newtype UserID = UserID {unUserID :: Integer}
  deriving (Eq, Ord, Show, Data, Enum, Typeable, Num)

$(deriveSafeCopy 0 'base ''UserID)

data User = User { _userID :: UserID
                 , _uName  :: String --user name
                 , _uEmail :: String --user email
                 , _uPass  :: String --user password
                 } deriving (Eq, Ord, Show, Data, Typeable)

$(makeLenses ''User)

$(deriveSafeCopy 0 'base ''User)

inferIxSet "UserDB" ''User 'noCalcs
  [ ''UserID
  , ''String
  , ''String
  , ''String
  ]

data Users =
  Users { _nextUserID :: UserID
        , _users      :: UserDB }
          deriving (Data, Typeable)

$(makeLenses ''Users)

$(deriveSafeCopy 0 'base ''Users)

initialUsersState :: Users
initialUsersState = Users (UserID 1) empty

-- create a new empty user
newUser :: Update Users User
newUser = do u <- get
             let user = User (u ^. nextUserID) "" "" ""
             put $ u & nextUserID %~ succ
                     & users      %~ IxSet.insert user
             return user

-- update a user by userID
updateUser :: User -> Update Users ()
updateUser updatedUser = do
  u <- get
  put $ u & users %~ IxSet.updateIx (updatedUser ^. userID) updatedUser

-- look up a user by id
userByID :: UserID -> Query Users (Maybe User)
userByID userId =
  do db <- ask
     return $ getOne $ (db ^. users) @= userId

-- look up a user by name
userByName :: String -> Query Users (Maybe User)
userByName name =
  do db <- ask
     return $ getOne $ (db ^. users) @= name

$(makeAcidic ''Users
  [ 'newUser
  , 'updateUser
  , 'userByID
  , 'userByName
  ])

