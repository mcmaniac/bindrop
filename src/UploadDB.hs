{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  TemplateHaskell, TypeFamilies, RecordWildCards,
  OverloadedStrings #-}

module UploadDB where

import Control.Monad.State  ( get, put )
import Control.Monad.Reader ( ask )
import Control.Lens         ( (&), (^.), (%~) )
import Control.Lens.TH      ( makeLenses )
import Data.Acid            ( Query, Update
                            , makeAcidic )
import Data.Data            ( Data, Typeable )
import Data.IxSet           ( Indexable(..), (@=), getOne
                            , inferIxSet, noCalcs )
import qualified Data.IxSet as IxSet
import Data.SafeCopy        ( base, deriveSafeCopy )
--import Data.Time            ( UTCTime(..), getCurrentTime )

newtype FileID = FileID {unFileID :: Integer}
  deriving (Eq, Ord, Show, Data, Enum, Typeable, Num)

$(deriveSafeCopy 0 'base ''FileID)

data FileUpload = FileUpload { _fileID :: FileID
                             , _fpath :: FilePath
                             , _fname :: String
--                             , time :: UTCTime
--                             , tags :: [String]
                             } deriving (Eq, Ord, Show, Data, Typeable)

$(makeLenses ''FileUpload)

$(deriveSafeCopy 0 'base ''FileUpload)

inferIxSet "FileDB" ''FileUpload 'noCalcs
  [ ''FileID
  , ''FilePath
  , ''String
  ]

data UploadDB =
  UploadDB { _nextFileID :: FileID
           , _files      :: FileDB }
           deriving (Data, Typeable)

$(makeLenses ''UploadDB)

$(deriveSafeCopy 0 'base ''UploadDB)

initialUploadDBState :: UploadDB
initialUploadDBState = UploadDB (FileID 1) empty

-- create a new empty file upload and add it to the DB
newUpload :: Update UploadDB FileUpload
newUpload = do f <- get
               let file = FileUpload (f ^. nextFileID) "" ""
               put $ f & nextFileID %~ succ
                       & files      %~ IxSet.insert file
               return file

-- update a file upload in the DB by fileID
updateUpload :: FileUpload -> Update UploadDB ()
updateUpload updatedFile = do
  f <- get
  put $ f & files %~ IxSet.updateIx (updatedFile ^. fileID) updatedFile

-- look up a file by id
fileByID :: FileID -> Query UploadDB (Maybe FileUpload)
fileByID fileId =
  do db <- ask
     return $ getOne $ (db ^. files) @= fileId

--TODO: Order by time using Proxy type

$(makeAcidic ''UploadDB
  [ 'newUpload
  , 'updateUpload
  , 'fileByID
  ])
