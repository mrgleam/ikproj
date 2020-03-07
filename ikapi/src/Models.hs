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
                                                )
import           Database.Persist.TH            ( mkMigrate
                                                , mkPersist
                                                , persistLowerCase
                                                , share
                                                , sqlSettings
                                                )

import           Config                         ( Config
                                                , configPool
                                                )
import           Data.Text                      ( Text )
import           Data.Int                       ( Int64 )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    email Text
    UniqueEmail email
    deriving Show Eq

Credential json
    user      UserId
    passHash  String
    deriving Show Eq
|]

instance ToJWT User
instance FromJWT User

data DataClaims = DataClaims { id :: Int64, username :: Text }
        deriving (Eq, Show)

$(deriveJSON defaultOptions ''DataClaims)

instance ToJWT DataClaims
instance FromJWT DataClaims

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool
