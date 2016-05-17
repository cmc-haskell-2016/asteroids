{-# LANGUAGE OverloadedStrings #-}

module ServerSide where


import API
import Game
import Types

import Control.Monad (forever)
import Control.Monad.Trans.Except
import Control.Concurrent
import Control.Concurrent.STM
import Data.Text
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types.Status
import Network.Wai
import qualified Network.WebSockets as WS
import Servant


type ServantResponse a = ExceptT ServantErr IO a


serveGame :: Server GameAPI
serveGame = newGame :<|> saveGame

newGame :: ServantResponse GameState
newGame = return initGame

saveGame :: ServantResponse GameId
saveGame = return 0

serveService :: Server ServiceAPI
serveService = openWebSocket

openWebSocket :: Application
openWebSocket =
    (websocketsOr WS.defaultConnectionOptions wsApp backupApp)
    where
        wsApp :: WS.ServerApp
        wsApp pending_conn = do
            conn <- WS.acceptRequest pending_conn
            putStrLn "new client"

            forever $ do
                -- to delete
                threadDelay(1000)
                -- action <- WS.receiveData conn
                return ()
                -- process user's action


        backupApp :: Application
        backupApp _ respond = do
            respond $ responseLBS status400 [] "Not a WebSocket request"


-- serverShipAccel :: GameState -> GameState
-- serverShipAccel GameOver = GameOver
-- serverShipAccel game@Game{..} = do
--     -- send this to server
--     -- game {
--     --     ship = ship {shipAccel = True}
--     -- }
--     game

-- serverShipNoAccel :: GameState -> GameState
-- serverShipNoAccel GameOver = GameOver
-- serverShipNoAccel game@Game{..} = do
--     -- send this to server
--     -- game {
--     --     ship = ship {shipAccel = False}
--     -- }
--     game

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
