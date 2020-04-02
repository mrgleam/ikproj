{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Api.Mission where

import           Data.Aeson
import           Data.Aeson.TH
import           Control.Monad
import           Control.Monad.Except           ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Logger           ( logDebugNS )
import           Data.Int                       ( Int64 )
import           Database.Persist.Postgresql    ( Entity(..)
                                                , selectFirst
                                                , insertUnique
                                                , update
                                                , insert
                                                , (==.)
                                                , (=.)
                                                )
import           Database.Persist.Sql           ( toSqlKey
                                                , fromSqlKey
                                                )
import           Database.Persist.Class         ( replaceUnique
                                                , get
                                                )
import           Servant
import           Servant.Auth.Server

import           Config                         ( AppT(..) )
import           Control.Monad.Metrics          ( increment )
import           Data.IORef                     ( readIORef )
import           Data.HashMap.Lazy              ( HashMap )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( getCurrentTime )
import           Lens.Micro                     ( (^.) )
import           Models.Database                ( UserMissionSetting
                                                  ( UserMissionSetting
                                                  )
                                                , runDb
                                                , userMissionSettingValue
                                                , userMissionSettingUser
                                                , DataClaims
                                                )
import qualified Models.Database               as Md

data UserMissionSettingRequest = UserMissionSettingRequest { mission :: Int64, value :: Int }
        deriving (Eq, Show)

$(deriveJSON defaultOptions 'UserMissionSettingRequest)

type Protected
  = "users" :> "mission" :> "setting" :> ReqBody '[JSON] UserMissionSettingRequest :> Post '[JSON] Int64

protected :: MonadIO m => AuthResult DataClaims -> ServerT Protected (AppT m)
protected (Authenticated (Md.DataClaims uid)) =
  checkAndUpsertUserMissionSetting uid
protected _ = throwAll err401

type UserMissionSettingAPI auths = (Auth auths DataClaims :> Protected)

userMissionSettingApi :: Proxy (UserMissionSettingAPI '[Cookie])
userMissionSettingApi = Proxy

userMissionSettingApiServer
  :: MonadIO m => ServerT (UserMissionSettingAPI auths) (AppT m)
userMissionSettingApiServer = protected

checkAndUpsertUserMissionSetting
  :: MonadIO m => Int64 -> UserMissionSettingRequest -> AppT m Int64
checkAndUpsertUserMissionSetting uid setting = do
  increment "upsertUserMissionSetting"
  logDebugNS "web" "upserting a user mission setting"
  fetchMission
    >=> upsertUserMissionSetting uid (value setting)
    $ mission setting

upsertUserMissionSetting :: MonadIO m => Int64 -> Int -> Int64 -> AppT m Int64
upsertUserMissionSetting uid value mid = do
  maybeUserMissionSetting <- runDb
    (selectFirst
      [ Md.UserMissionSettingUser ==. toSqlKey uid
      , Md.UserMissionSettingMission ==. toSqlKey mid
      ]
      []
    )
  case maybeUserMissionSetting of
    Nothing -> do
      now                   <- liftIO getCurrentTime
      newUserMissionSetting <- runDb
        (insertUnique
          (UserMissionSetting (toSqlKey uid)
                              (toSqlKey mid)
                              value
                              now
                              now
          )
        )
      case newUserMissionSetting of
        Nothing ->
          throwError err403 { errBody = "The system can't insert data." }
        Just newSetting -> return $ fromSqlKey newSetting
    Just (Entity id _) -> do
      now <- liftIO getCurrentTime
      _   <- runDb
        (update
          id
          [ Md.UserMissionSettingValue =. value
          , Md.UserMissionSettingUpdated =. now
          ]
        )
      return $ fromSqlKey id

fetchMission :: MonadIO m => Int64 -> AppT m Int64
fetchMission missionId = do
  (Entity uid _) <-
    maybe (throwError err404 { errBody = "Mission not found." }) return
      =<< runDb (selectFirst [Md.MissionId ==. toSqlKey missionId] [])
  return $ fromSqlKey uid
