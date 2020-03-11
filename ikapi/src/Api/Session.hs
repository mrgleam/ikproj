{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Session where

import           Control.Monad.Except        (MonadIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment)
import           Models                      (DataClaims)

import           Servant
import           Servant.Auth.Server

import           Config                      (AppT (..))

session :: MonadIO m => AppT m NoContent
session = do
  increment "session"
  logDebugNS "web" "session"
  return NoContent

type Protected = "session" :> Get '[JSON] NoContent

protected :: MonadIO m => AuthResult DataClaims -> ServerT Protected (AppT m)
protected (Authenticated user) = session
protected _ = throwAll err401

type SessionAPI auths = (Auth auths DataClaims :> Protected)

sessionApi :: Proxy (SessionAPI '[Cookie])
sessionApi = Proxy

sessionServer :: MonadIO m => ServerT (SessionAPI auths) (AppT m)
sessionServer = protected