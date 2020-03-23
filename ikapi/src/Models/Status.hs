{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Status where

import           Data.Aeson
import           GHC.Generics
import           Database.Persist.TH
import           Prelude

data Status = Active | Inactive
    deriving (Show, Read, Eq, Generic)
derivePersistField "Status"

instance ToJSON Status
instance FromJSON Status
