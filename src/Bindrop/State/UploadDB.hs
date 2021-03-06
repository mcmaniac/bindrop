{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Bindrop.State.UploadDB where

import Control.Lens.TH      ( makeLenses )
import Data.Acid
import Data.Data            ( Data, Typeable )
import Data.IxSet           ( Indexable(..), (@=), (@<), getOne
                            , inferIxSet, noCalcs, toDescList)
import qualified Data.IxSet as IxSet
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.Time            ( UTCTime(..) )

newtype FileID = FileID {unFileID :: Integer}
  deriving (Eq, Ord, Show, Data, Enum, Typeable, Num)

newtype UserName = UserName {unUserName :: String}
  deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''FileID)
$(deriveSafeCopy 0 'base ''UserName)

data FileUpload = FileUpload { _fileID     :: FileID
                             , _fpath      :: FilePath --path on server disk
                             , _fname      :: String   --original uploaded name
                             , _sfname     :: String   --random name on server disk
                             , _uploadTime :: UTCTime
                             , _public     :: Bool     --privacy setting
                             , _userName   :: UserName --uploader name
                             , _dlCount    :: Int      --download count
                             } deriving (Eq, Ord, Show, Data, Typeable)

$(makeLenses ''FileUpload)

$(deriveSafeCopy 0 'base ''FileUpload)

inferIxSet "FileDB" ''FileUpload 'noCalcs
  [ ''FileID
  , ''FilePath
  , ''String
  , ''String
  , ''UTCTime
  , ''Bool
  , ''UserName
  , ''Int
  ]

data UploadDB =
  UploadDB { _nextFileID :: FileID
           , _files      :: FileDB }
           deriving (Data, Typeable)

$(makeLenses ''UploadDB)

$(deriveSafeCopy 0 'base ''UploadDB)

initialUploadDBState :: UploadDB
initialUploadDBState = UploadDB (FileID 1) empty

