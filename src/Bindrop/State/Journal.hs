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

newtype EntryID = EntryID {unEntryID :: Integer}
  deriving (Eq, Ord, Show, Data, Enum, Typeable, Num)

$(deriveSafeCopy 0 'base ''EntryID)

data Journal = Journal { _entryID :: EntryID
                       , _acount  :: Int --account count
                       , _ucount  :: Int --upload count
                       , _dcount  :: Int --download count
                       } deriving (Eq, Ord, Show, Data, Typeable)

$(makeLenses ''Journal)

$(deriveSafeCopy 0 'base ''Journal)

inferIxSet "EntryDB" ''Journal 'noCalcs
  [ ''EntryID
  , ''Int
  , ''Int
  , ''Int
  ]

data JournalDB =
  JournalDB { _nextEntryID :: EntryID
            , _entries     :: EntryDB
            } deriving (Data, Typeable)

$(makeLenses ''JournalDB)

$(deriveSafeCopy 0 'base ''JournalDB)

initialJournalDBState :: JournalDB
initialJournalDBState = JournalDB (EntryID 1) empty

