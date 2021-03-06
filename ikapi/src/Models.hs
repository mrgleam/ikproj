{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Data.Aeson
import           Data.Aeson.TH
import           Servant.Auth.Server
import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                , asks
                                                , liftIO
                                                )
import           Database.Persist.Sql           ( SqlPersistT
                                                , runMigration
                                                , runSqlPool
                                                , insert
                                                )
import           Database.Persist.TH            ( mkMigrate
                                                , mkPersist
                                                , persistLowerCase
                                                , share
                                                , sqlSettings
                                                , derivePersistField
                                                )

import           Config                         ( Config
                                                , configPool
                                                )
import           Data.Text                      ( Text )
import           Data.Int                       ( Int64 )
import           Data.Time
import           Models.Status

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name String 
    email String
    UniqueEmail email
    deriving Show Eq

Credential json
    user      UserId
    passHash  String
    deriving Show Eq

Mission json
    name Text
    status Status
    deriving Show Eq

UserMissionSetting json
    user      UserId
    mission   MissionId
    value     Int
    updated   UTCTime default=now()
    created   UTCTime default=now()
    deriving Show Eq
|]

instance ToJWT User
instance FromJWT User

newtype DataClaims = DataClaims { id :: Int64 }
        deriving (Eq, Show)

$(deriveJSON defaultOptions ''DataClaims)

instance ToJWT DataClaims
instance FromJWT DataClaims

doMigrations :: SqlPersistT IO ()
doMigrations = do
                runMigration migrateAll
                insert (Mission "Reading" Active)
                return ()

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool
