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

import Control.Monad (forever)
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
    conns :: [WS.Connection]
}


serveGame :: Server GameAPI
serveGame = newGame :<|> saveGame

newGame :: ServantResponse GameState
newGame = return (InGame initUniverse)

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
            putStrLn "new client"
            addClient ss conn

            forever $ do
                action <- WS.receiveData conn
                editGameState action ss


        backupApp :: Application
        backupApp _ respond = do
            respond $ responseLBS status400 [] "Not a WebSocket request"


addClient :: TVar ServerState -> WS.Connection -> IO ()
addClient ss conn = do
    shared <- readTVarIO ss
    let new_ss = shared {
        conns = conn : (conns shared)
    }
    atomically $ writeTVar ss new_ss


editGameState :: Action -> TVar ServerState -> IO ()
editGameState action ss = do
    shared <- readTVarIO ss
    let new_ss = shared {
        game = handleActions action (game shared)
    }
    atomically $ writeTVar ss new_ss


handleActions :: Action -> GameState -> GameState
handleActions _ GameOver = GameOver
handleActions EnableAcceleration (InGame u@Universe{..}) =
    InGame u { ship = ship {shipAccel = True} }
handleActions DisableAcceleration (InGame u@Universe{..}) =
    InGame u { ship = ship {shipAccel = False} }
handleActions _ u = u


-- periodicUpdates TVar ServerState -> IO ()
-- periodicUpdates ss = do


-- serverShipRotateLeft :: GameState -> GameState
-- serverShipRotateLeft GameOver = GameOver
-- serverShipRotateLeft game@Game{..} =
--     -- send this to server
--     -- game {
--     --     ship = ship {rotation = (rotation ship) - 5}
--     -- }
--     game

-- serverShipRotateRight :: GameState -> GameState
-- serverShipRotateRight GameOver = GameOver
-- serverShipRotateRight game@Game{..} =
--     -- send this to server
--     -- game {
--     --     ship = ship {rotation = (rotation ship) + 5}
--     -- }
--     game

-- serverShieldOn :: GameState -> GameState
-- serverShieldOn GameOver = GameOver
-- serverShieldOn game@Game{..} =
--     -- send this to server
--     -- game {
--     --     ship = ship {shieldOn = True}
--     -- }
--     game

-- serverShieldOff :: GameState -> GameState
-- serverShieldOff GameOver = GameOver
-- serverShieldOff game@Game{..} =
--     -- send this to server
--     -- game {
--     --     ship = ship {shieldOn = False}
--     -- }
--     game

-- serverShoot :: GameState -> GameState
-- serverShoot GameOver = GameOver
-- serverShoot game@Game{..} =
--     -- send this to server
--     -- game {
--     --     bullets = (initBullet ship) : bullets
--     -- }
--     game
