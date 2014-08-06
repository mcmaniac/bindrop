{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  TemplateHaskell, TypeFamilies,
  OverloadedStrings #-}

module UploadDB where

import Control.Monad.State  ( get, put )
import Control.Monad.Reader ( ask )
import Control.Lens         ( (&), (^.), (%~) )
import Control.Lens.TH      ( makeLenses )
import Data.Acid
import Data.Data            ( Data, Typeable )
import Data.IxSet           ( Indexable(..), (@=), getOne
                            , inferIxSet, noCalcs, toDescList)
import qualified Data.IxSet as IxSet
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.Time            ( UTCTime(..) )

newtype FileID = FileID {unFileID :: Integer}
  deriving (Eq, Ord, Show, Data, Enum, Typeable, Num)

$(deriveSafeCopy 0 'base ''FileID)

data FileUpload = FileUpload { _fileID :: FileID
                             , _fpath :: FilePath --path on server disk
                             , _fname :: String  --original uploaded name
                             , _sfname :: String --random name on server disk
                             , _uploadTime :: UTCTime
                             } deriving (Eq, Ord, Show, Data, Typeable)

$(makeLenses ''FileUpload)

$(deriveSafeCopy 0 'base ''FileUpload)

inferIxSet "FileDB" ''FileUpload 'noCalcs
  [ ''FileID
  , ''FilePath
  , ''String
  , ''String
  , ''UTCTime
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
newUpload :: UTCTime -> Update UploadDB FileUpload
newUpload t = do f <- get
                 let file = FileUpload (f ^. nextFileID) "" "" "" t
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

-- look up a file by name
fileBySName :: String -> Query UploadDB (Maybe FileUpload)
fileBySName name =
  do db <- ask
     return $ getOne $ (db ^. files) @= name

$(makeAcidic ''UploadDB
  [ 'newUpload
  , 'updateUpload
  , 'fileByID
  , 'fileBySName
  ])

-- get a list of the 20 most recent uploads
mostRecentUploads :: (Typeable a, Indexable a) => IxSet.IxSet a -> [a]
mostRecentUploads acid = take 20 $ toDescList
  (IxSet.Proxy :: IxSet.Proxy UTCTime) acid

