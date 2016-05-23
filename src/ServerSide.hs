{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module ServerSide where


import API
import Game
import Types
import Interface
import Ship
import Bullet

import Control.Monad (forever, forM_)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except
import Control.Concurrent
import Control.Concurrent.STM
-- import Data.Text
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types.Status
import Network.Wai
import qualified Network.WebSockets as WS
import Servant


type ServantResponse a = ExceptT ServantErr IO a

data ServerState = ServerState {
    game :: GameState,
    clients :: [Client]
}

data Client = Client {
    conn :: WS.Connection,
    client_id :: ClientId
}

instance Show Client where
    show c = show (client_id c)

instance Eq Client where
    x == y = (client_id x) == (client_id y)
    x /= y = not $ x == y


serveGame :: TVar ServerState -> Server GameAPI
serveGame ss = (newGame ss) :<|> saveGame

newGame :: TVar ServerState -> ServantResponse GameState
newGame ss = do
    let new_game = (InGame initUniverse)
    liftIO $ atomically $ do
        shared <- readTVar ss
        let new_ss = shared {
            game = new_game
        }
        writeTVar ss new_ss

    return new_game

saveGame :: ServantResponse GameId
saveGame = return 0

serveService :: TVar ServerState -> Server ServiceAPI
serveService ss = openWebSocket ss

openWebSocket :: TVar ServerState -> Application
openWebSocket ss =
    (websocketsOr WS.defaultConnectionOptions wsApp backupApp)
    where
        wsApp :: WS.ServerApp
        wsApp pending_conn = do
            conn <- WS.acceptRequest pending_conn
            client <- addClient ss conn
            putStrLn $ "Client " ++ show (client_id client) ++ " has connected"

            forever $ do
                action <- WS.receiveData conn
                print action
                checkCloseRequest action ss client
                atomically $ do
                    shared <- readTVar ss
                    writeTVar ss shared {
                        game = editGameState action (game shared)
                    }
                foo <- readTVarIO ss
                print (game foo)

        backupApp :: Application
        backupApp _ respond = do
            respond $ responseLBS status400 [] "Not a WebSocket request"


checkCloseRequest :: Action -> TVar ServerState -> Client -> IO ()
checkCloseRequest Finish ss client = atomically $ do
    shared <- readTVar ss
    let updated_clients = filter (\c -> client /= c) (clients shared)
    writeTVar ss shared {
        clients = updated_clients
    }
    return ()
checkCloseRequest _ _ _ = return ()


addClient :: TVar ServerState -> WS.Connection -> IO Client
addClient ss conn = atomically $ do
    shared <- readTVar ss
    let c = clients shared
    let new_client = Client conn (newClientId c)
    let new_ss = shared {
        clients = (new_client : c)
    }
    writeTVar ss new_ss
    return new_client


newClientId :: [Client] -> ClientId
newClientId clients = helper clients clients 0
    where
        helper :: [Client] -> [Client] -> ClientId -> ClientId
        helper _ [] n = n
        helper full_clients (x:xs) n
            | (client_id x) == n = helper full_clients full_clients (n+1)
            | otherwise = helper full_clients xs n


editGameState :: Action -> GameState -> GameState
editGameState action gs = handleActions action gs


handleActions :: Action -> GameState -> GameState
handleActions EnableAcceleration (InGame u@Universe{..}) =
    InGame u {ship = ship {shipAccel = True}}
handleActions DisableAcceleration (InGame u@Universe{..}) =
    InGame u {ship = ship {shipAccel = False}}
handleActions RotateLeft (InGame u@Universe{..}) =
    InGame u {ship = ship {rotation = (rotation ship) - 5}}
handleActions RotateRight (InGame u@Universe{..}) =
    InGame u {ship = ship {rotation = (rotation ship) + 5}}
handleActions StopRotatingLeft (InGame u@Universe{..}) =
    InGame u {ship = ship {rotation = (rotation ship) + 5}}
handleActions StopRotatingRight (InGame u@Universe{..}) =
    InGame u {ship = ship {rotation = (rotation ship) - 5}}
handleActions EnableShield (InGame u@Universe{..}) =
    InGame u  {ship = ship {shieldOn = True}}
handleActions DisableShield (InGame u@Universe{..}) =
    InGame u  {ship = ship {shieldOn = False}}
handleActions Shoot (InGame u@Universe{..}) =
    InGame u {bullets = (initBullet ship) : bullets}
handleActions _ u = u


periodicUpdates :: TVar ServerState -> IO ()
periodicUpdates ss = forever $ do
    let fps = 60
    threadDelay (1000000 `div` fps)
    shared <- readTVarIO ss
    let updated_game = updateGame (1 / fromIntegral fps) (game shared)
    forM_ (clients shared) (applySendToClient updated_game)

    let new_ss = shared {
        game = updated_game
    }
    atomically $ writeTVar ss new_ss
    where
        applySendToClient :: GameState -> Client -> IO ()
        applySendToClient g client = WS.sendBinaryData (conn client) g
