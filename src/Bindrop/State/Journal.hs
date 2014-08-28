{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Bindrop.State.Journal where

import Control.Lens.TH      ( makeLenses )
import Data.Acid
import Data.Data            ( Data, Typeable )
import Data.IxSet           ( Indexable(..), (@=), (@<), getOne
                            , inferIxSet, noCalcs, toDescList)
import qualified Data.IxSet as IxSet
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.Time            ( UTCTime(..) )

data Journal = Journal { _acount :: Int --account count
                       , _ucount :: Int --upload count
                       , _dcount :: Int --download count
                       } deriving (Eq, Ord, Show, Data, Typeable)

$(makeLenses ''Journal)

$(deriveSafeCopy 0 'base ''Journal)

inferIxSet "JournalDB" ''Journal 'noCalcs
  [ ''Int
  , ''Int
  , ''Int
  ]

data JournaldDB =
  JournalDB { _entries :: JournalDB } deriving (Data, Typeable)

$(makeLenses ''JournalDB)

$(deriveSafeCopy 0 'base ''JournalDB)

initialUploadDBState :: JournalDB
initialUploadDBState = JournalDB 0 0 0

