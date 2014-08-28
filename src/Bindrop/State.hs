{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Bindrop.State where

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
import Data.Time            ( UTCTime(..) )

import Bindrop.State.Journal
import Bindrop.State.UploadDB
import Bindrop.State.Users

data BindropState =
  BindropState { _fileDB :: UploadDB
               , _userDB :: Users
               } deriving (Data, Typeable)

$(makeLenses ''BindropState)

$(deriveSafeCopy 0 'base ''BindropState)

inferIxSet "BindropDBs" ''BindropState 'noCalcs
  [ ''FileDB
  , ''UserDB
  ]

initialBindropState :: BindropState
initialBindropState = BindropState (initialUploadDBState) (initialUsersState)

-- UploadDB
-- create a new empty file upload and add it to the DB
newUpload :: UTCTime -> Update BindropState FileUpload
newUpload t = do
  f <- get
  let file = FileUpload (f ^. fileDB . nextFileID) "" "" "" t True
                        (UserName "") 0
  put $ f & fileDB . nextFileID %~ succ
          & fileDB . files      %~ IxSet.insert file
  return file

-- update a file upload in the DB by fileID
updateUpload :: FileUpload -> Update BindropState ()
updateUpload updatedFile = do
  f <- get
  put $ f & fileDB . files %~ IxSet.updateIx (updatedFile ^. fileID) updatedFile

-- look up a file by id
fileByID :: FileID -> Query BindropState (Maybe FileUpload)
fileByID fileId = do
  db <- ask
  return $ getOne $ (db ^. fileDB . files) @= fileId

-- look up a file by name
fileBySName :: String -> Query BindropState (Maybe FileUpload)
fileBySName name = do
  db <- ask
  return $ getOne $ (db ^. fileDB . files) @= name


-- get a list of the 20 most recent uploads
mostRecentUploads :: UTCTime -> Query BindropState [FileUpload]
mostRecentUploads now = do
  db <- ask
  let files' =
        take 20 $
        filter (\x -> x ^. public == True) $
        toDescList
        (IxSet.Proxy :: IxSet.Proxy UTCTime) $
        (db ^. fileDB . files) @< now
  return files'

uploadsByUserName :: UserName -> Query BindropState [FileUpload]
uploadsByUserName u = do
  db <- ask
  let files' = toDescList
        (IxSet.Proxy :: IxSet.Proxy UTCTime) $
        (db ^. fileDB . files) @= u
  return files'

-- Users
-- create a new empty user
newUser :: Update BindropState User
newUser = do
  u <- get
  let user = User (u ^. userDB . nextUserID) "" "" "" 0
  put $ u & userDB . nextUserID %~ succ
          & userDB . users      %~ IxSet.insert user
  return user

-- update a user by userID
updateUser :: User -> Update BindropState ()
updateUser updatedUser = do
  u <- get
  put $ u & userDB . users %~ IxSet.updateIx
    (updatedUser ^. userID) updatedUser

-- look up a user by id
userByID :: UserID -> Query BindropState (Maybe User)
userByID userId = do
 db <- ask
 return $ getOne $ (db ^. userDB . users) @= userId

-- look up a user by name
userByName :: String -> Query BindropState (Maybe User)
userByName name = do
  db <- ask
  return $ getOne $ (db ^. userDB . users) @= name

-- look up a user by email
userByEmail :: String -> Query BindropState (Maybe User)
userByEmail email = do
  db <- ask
  return $ getOne $ (db ^. userDB . users) @= email

$(makeAcidic ''BindropState
  [ 'newUpload
  , 'updateUpload
  , 'fileByID
  , 'fileBySName
  , 'mostRecentUploads
  , 'uploadsByUserName
  , 'newUser
  , 'updateUser
  , 'userByID
  , 'userByName
  , 'userByEmail
  ])

