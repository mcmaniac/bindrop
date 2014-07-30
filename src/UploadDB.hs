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
                             , name :: String
                             } deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''FileUpload)

newtype UploadPath = UploadPath FilePath
  deriving (Eq, Ord, Data, Typeable, SafeCopy)

newtype UploadName = UploadName String
  deriving (Eq, Ord, Data, Typeable, SafeCopy)

