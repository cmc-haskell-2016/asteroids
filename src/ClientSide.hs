{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientSide where

import API
import Game
import Types

import Control.Monad (forever)
import Control.Monad.Trans.Except
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.ByteString.Lazy.Internal (ByteString)
import qualified Network.WebSockets as WS
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Media.MediaType (MediaType)
import qualified Network.HTTP.Types.Header as H
import Network.HTTP.Types.Method
import Servant
import Servant.Client
import System.Exit (die)

type ServantResponse a = ExceptT ServantError IO a

data ClientAPI = ClientAPI {
    new :: ServantResponse GameState,
    save :: ServantResponse GameId,
    open_socket :: Method -> ServantResponse (Int, ByteString, MediaType, [H.Header], Response ByteString)
}

data ClientState = ClientState {
    game :: TVar GameState,
    conn :: WS.Connection,
    http_client :: ClientAPI
}


serverUrl :: Ip -> Port -> BaseUrl
serverUrl ip port = BaseUrl Http ip port ""


mkAPIClient :: BaseUrl -> Manager -> Client ServerAPI
mkAPIClient = client (Proxy :: Proxy ServerAPI)


mkAsteroidsClient :: Ip -> Port -> Manager -> IO ClientAPI
mkAsteroidsClient ip port manager = return ClientAPI{..}
  where
    gameClient :<|> serviceClient = mkAPIClient (serverUrl ip port) manager

    (new :<|> save) = gameClient
    open_socket = serviceClient


mkClientWithManager :: Ip -> Port -> IO ClientAPI
mkClientWithManager ip port = do
    manager <- newManager defaultManagerSettings
    mkAsteroidsClient ip port manager


getServerResponse :: Either a b -> IO b
getServerResponse (Left _) = die "Unexpected server response"
getServerResponse (Right r) = return r


getUpdatedGameState :: Time -> ClientState -> IO ClientState
getUpdatedGameState _ cs = return cs


handleUpdates :: ClientState -> IO ()
handleUpdates state@ClientState{..} = do
    _ <- forkIO $ forever $ do
        game_state <- WS.receiveData conn
        atomically $ writeTVar game game_state

    return ()

restartGame :: ClientState -> IO ClientState
restartGame cs = do
    let c = http_client cs
    response <- runExceptT (new c)
    new_game <- getServerResponse response
    atomically $ writeTVar (game cs) new_game
    return cs
