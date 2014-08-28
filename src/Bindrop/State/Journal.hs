{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Bindrop.State.Journal where

import Control.Lens.TH      ( makeLenses )
import Data.Acid
import Data.Data            ( Data, Typeable )
import qualified Data.IxSet as IxSet
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.Time            ( UTCTime(..) )

data Journal = Journal { _acount  :: Int --account count
                       , _ucount  :: Int --upload count
                       , _dcount  :: Int --download count
                       } deriving (Eq, Ord, Show, Data, Typeable)

$(makeLenses ''Journal)
$(deriveSafeCopy 0 'base ''Journal)

initialJournalState :: Journal
initialJournalState = Journal 0 0 0

