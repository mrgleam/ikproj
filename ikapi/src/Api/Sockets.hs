{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Sockets where

import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)

import           Servant
import           Servant.Auth.Server

import           Config                      (AppT (..))
import           Lens.Micro                  ((^.))
import           Models.Database             (User (User), runDb, userEmail,
                                              userName, DataClaims)

import           Servant.API.WebSocket          (WebSocketPending)
import           Network.WebSockets             ( Connection
                                                , PendingConnection
                                                , forkPingThread
                                                , sendTextData
                                                , acceptRequest
                                                )
import           Control.Concurrent             ( threadDelay )
import           Data.Foldable                  ( forM_ )
import           Data.Text                      ( pack )

type Sockets =
  "echo" :> WebSocketPending

type SocketsAPI = Sockets

socketsApi :: Proxy SocketsAPI
socketsApi = Proxy

socketsServer :: MonadIO m => ServerT SocketsAPI (AppT m)
socketsServer = echo

echo :: MonadIO m => PendingConnection -> AppT m ()
echo pConn = do
      logDebugNS "socket" "echo"
      c <- liftIO $ acceptRequest pConn
      liftIO $ forkPingThread c 10
      liftIO . forM_ [1..] $ \i ->
        sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000
