{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module ServerSide
(
    serveGame,
    serveService,
    periodicUpdates,
    ServerState(..),
    Client(..)
)
where


import API
import Game
import Types
import Interface
import Ship
import Bullet
import Universe

import Control.Exception (try)
import Control.Monad (forever, filterM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types.Status
import Network.Wai
import qualified Network.WebSockets as WS
import Servant


type ServantResponse a = ExceptT ServantErr IO a

data ServerState = ServerState {
    game :: GameMode,
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
    liftIO $ atomically $ do
        shared <- readTVar ss
        let new_ss = shared {
            game = modeHandler (length (clients shared)) (game shared)
        }
        writeTVar ss new_ss
    shared <- liftIO $ readTVarIO ss
    return $ getGameState $ game $ shared
    where
        modeHandler :: Int -> GameMode -> GameMode
        modeHandler n (Single gs) = modeHandlerSingle n gs
        modeHandler n (Cooperative gs) = modeHandlerCooperative n gs

        modeHandlerSingle :: Int -> GameState -> GameMode
        modeHandlerSingle 1 _ = Single $ InGame [initUniverse 1]
        modeHandlerSingle _ _ = Single $ GameOver

        modeHandlerCooperative :: Int -> GameState -> GameMode
        modeHandlerCooperative n _ = defaultCooperative n

        defaultCooperative :: Int -> GameMode
        defaultCooperative n
            | n > 2 = Cooperative $ GameOver
            | n == 2 = Cooperative $ InGame [initUniverse 2]
            | otherwise = Cooperative $ Waiting (2 - n)

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
                checkCloseRequest action ss client
                --checkPauseRequest action ss
                atomically $ do
                    shared <- readTVar ss
                    writeTVar ss shared {
                        game = handleActions (client_id client) action (game shared)
                    }

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


handleActions :: Int -> Action -> GameMode -> GameMode
handleActions _ action (Single gs) =
    Single $ handleActionsSingle action gs
handleActions client_id action (Cooperative gs) =
    Cooperative $ handleActionsCooperative client_id action gs


handleActionsSingle :: Action -> GameState -> GameState
handleActionsSingle EnableAcceleration (InGame (u@Universe{..}:_)) =
    InGame [u {ships =  map (\s -> s {shipAccel = True}) ships}]
handleActionsSingle DisableAcceleration (InGame (u@Universe{..}:_)) =
    InGame [u {ships = map (\s -> s {shipAccel = False}) ships}]
handleActionsSingle RotateLeft (InGame (u@Universe{..}:_)) =
    InGame [u {ships = map (\s -> s {rotation = (rotation $ s) - 5}) ships}]
handleActionsSingle RotateRight (InGame (u@Universe{..}:_)) =
    InGame [u {ships = map (\s -> s {rotation = (rotation $ s) + 5}) ships}]
handleActionsSingle StopRotatingLeft (InGame (u@Universe{..}:_)) =
    InGame [u {ships = map (\s -> s {rotation = (rotation $ s) + 5}) ships}]
handleActionsSingle StopRotatingRight (InGame (u@Universe{..}:_)) =
    InGame [u {ships = map (\s -> s {rotation = (rotation $ s) - 5}) ships}]
handleActionsSingle EnableShield (InGame (u@Universe{..}:_)) =
    InGame [u  {ships = map (\s -> s {shieldOn = True}) ships}]
handleActionsSingle DisableShield (InGame (u@Universe{..}:_)) =
    InGame [u  {ships = map (\s -> s {shieldOn = False}) ships}]
handleActionsSingle Shoot (InGame (u@Universe{..}:_)) =
    InGame [u {bullets = (initBullet $ head ships) : bullets}]
handleActionsSingle _ u = u


--todo
handleActionsCooperative :: Int -> Action -> GameState -> GameState
handleActionsCooperative n EnableAcceleration (InGame (u@Universe{..}:_)) =
    InGame [u {ships = map (\s -> if s == ships!!n then s {shipAccel = True} else s) ships}]
handleActionsCooperative n DisableAcceleration (InGame (u@Universe{..}:_)) =
    InGame [u {ships = map (\s -> if s == ships!!n then s {shipAccel = False} else s) ships}]
handleActionsCooperative n RotateLeft (InGame (u@Universe{..}:_)) =
    InGame [u {ships = map (\s -> if s == ships!!n then s {rotation = (rotation $ s) - 5} else s) ships}]
handleActionsCooperative n RotateRight (InGame (u@Universe{..}:_)) =
    InGame [u {ships = map (\s -> if s == ships!!n then s {rotation = (rotation $ s) + 5} else s) ships}]
handleActionsCooperative n StopRotatingLeft (InGame (u@Universe{..}:_)) =
    InGame [u {ships = map (\s -> if s == ships!!n then s {rotation = (rotation $ s) + 5} else s) ships}]
handleActionsCooperative n StopRotatingRight (InGame (u@Universe{..}:_)) =
    InGame [u {ships = map (\s -> if s == ships!!n then s {rotation = (rotation $ s) - 5} else s) ships}]
handleActionsCooperative n EnableShield (InGame (u@Universe{..}:_)) =
    InGame [u  {ships = map (\s -> if s == ships!!n then s {shieldOn = True} else s) ships}]
handleActionsCooperative n DisableShield (InGame (u@Universe{..}:_)) =
    InGame [u  {ships = map (\s -> if s == ships!!n then s {shieldOn = False} else s) ships}]
handleActionsCooperative n Shoot (InGame (u@Universe{..}:_)) =
    InGame [u {bullets = (initBullet $ ships !! n) : bullets}]
handleActionsCooperative _ _ u = u


periodicUpdates :: TVar ServerState -> IO ()
periodicUpdates ss = forever $ do
    threadDelay (1000000 `div` fps)
    atomically $ do
        shared <- readTVar ss
        writeTVar ss shared {
            game = updateGame (1 / fromIntegral fps) (game shared)
        }
    shared <- readTVarIO ss
    updated_clients <- filterM (sendGameToClient (gs shared)) (clients shared)
    atomically $ do
        shared1 <- readTVar ss
        writeTVar ss shared1 {
            clients = updated_clients
        }
    where
        gs = (getGameState . game)
        sendGameToClient :: GameState -> Client -> IO Bool
        sendGameToClient g client = do
            res <- try send :: IO (Either WS.ConnectionException ())
            case res of
                Left _ex -> return False
                Right _ok -> return True
            where
                send = (WS.sendBinaryData (conn client) g)
        fps = 60