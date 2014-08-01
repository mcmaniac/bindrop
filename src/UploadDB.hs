{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  TemplateHaskell, TypeFamilies, RecordWildCards,
  OverloadedStrings #-}

module UploadDB where

import Control.Monad.State  ( get, put )
import Control.Monad.Reader ( ask )
import Data.Acid            ( Query, Update
                            , makeAcidic )
import Data.Data            ( Data, Typeable )
import Data.IxSet           ( Indexable(..), IxSet(..), (@=)
                            , Proxy(..), getOne, ixFun, ixSet )
import qualified Data.IxSet as IxSet
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
--import Data.Time            ( UTCTime(..), getCurrentTime )

newtype FileID = FileID {unFileID :: Integer}
  deriving (Eq, Ord, Show, Data, Enum, Typeable, SafeCopy)

data FileUpload = FileUpload { fileID :: FileID
                             , fpath :: FilePath
                             , fname :: String
--                             , time :: UTCTime
--                             , tags :: [String]
                             } deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''FileUpload)

newtype UploadPath = UploadPath FilePath
  deriving (Eq, Ord, Data, Typeable, SafeCopy)

newtype UploadName = UploadName String
  deriving (Eq, Ord, Data, Typeable, SafeCopy)

--newtype UploadTime = UploadTime UTCTime
--  deriving (Eq, Ord, Data, Typeable, SafeCopy)

--newtype UploadTag = UploadTag Text
--  deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Indexable FileUpload where
  empty = ixSet
    [ ixFun $ \fu -> [fileID fu]
    , ixFun $ \fu -> [UploadPath $ fpath fu]
    , ixFun $ \fu -> [UploadName $ fname fu]
    --, ixFun $ \fu -> [UploadTime $ time fu]
    --, ixFun $ \fu -> map UploadTag (tags fu)
    ]

data UploadDB =
  UploadDB { nextFileID :: FileID
           , files      :: IxSet FileUpload }
           deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''UploadDB)

initialUploadDBState :: UploadDB
initialUploadDBState =
  UploadDB { nextFileID = FileID 1
           , files      = empty }

-- create a new empty file upload and add it to the DB
newUpload :: Update UploadDB FileUpload
newUpload = do f@UploadDB{..} <- get
               let file = FileUpload { fileID = nextFileID
                                     , fpath = ""
                                     , fname = ""
                                     }
               put $ f { nextFileID = succ nextFileID
                       , files      = IxSet.insert file files
                       }
               return file

-- update a file upload in the DB by fileID
updateUpload :: FileUpload -> Update UploadDB ()
updateUpload updatedFile = do
  f@UploadDB{..} <- get
  put $ f { files =
              IxSet.updateIx (fileID updatedFile) updatedFile files
          }

-- look up a file by id
fileByID :: FileID -> Query UploadDB (Maybe FileUpload)
fileByID fileId =
  do UploadDB{..} <- ask
     return $ getOne $ files @= fileId

--TODO: Order by time using Proxy type

$(makeAcidic ''UploadDB
  [ 'newUpload
  , 'updateUpload
  , 'fileByID
  ])

