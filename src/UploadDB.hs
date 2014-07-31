{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  TemplateHaskell, TypeFamilies, RecordWildCards,
  OverloadedStrings #-}

module UploadDB where

import Control.Applicative  ( (<$>) )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.Data            ( Data, Typeable )
import Data.IxSet           ( Indexable(..), IxSet(..), (@=)
                            , Proxy(..), getOne, ixFun, ixSet )
import qualified Data.IxSet as IxSet
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
import Data.Text            ( Text )
import Data.Text.Lazy       ( toStrict )
import qualified Data.Text as Text
import Data.Time            ( UTCTime(..), getCurrentTime )

data FileUpload = FileUpload { path :: FilePath
                             , name :: Text
--                             , time :: UTCTime
--                             , tags :: [Text]
                             } deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''FileUpload)

newtype UploadPath = UploadPath FilePath
  deriving (Eq, Ord, Data, Typeable, SafeCopy)

newtype UploadName = UploadName Text
  deriving (Eq, Ord, Data, Typeable, SafeCopy)

--newtype UploadTime = UploadTime UTCTime
--  deriving (Eq, Ord, Data, Typeable, SafeCopy)

--newtype UploadTag = UploadTag Text
--  deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Indexable FileUpload where
  empty = ixSet
    [ ixFun $ \bp -> [UploadPath $ path bp]
    , ixFun $ \bp -> [UploadName $ name bp]
    --, ixFun $ \bp -> [UploadTime $ time bp]
    --, ixFun $ \bp -> map UploadTag (tags bp)
    ]

data UploadDB = UploadDB { files :: IxSet FileUpload }
  deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''UploadDB)

initialUploadDBState :: UploadDB
initialUploadDBState = UploadDB { files = empty }

