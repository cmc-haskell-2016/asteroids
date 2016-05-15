{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module API
(
    serveGame,
    serveService,
    ServerAPI,
    GameAPI
) where

import Types
import Game

import Control.Monad.Trans.Except
import Data.Text
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types.Status
import Network.Wai
import Network.WebSockets
import Servant


type ServantResponse a = ExceptT ServantErr IO a


type ServerAPI
    = "game" :> GameAPI
    :<|> "service" :> ServiceAPI


type ServiceAPI
    = "open_socket" :> Raw


type GameAPI
    = "new" :> Get '[JSON] GameState
    :<|> "save" :> Get '[JSON] GameId


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
    (websocketsOr defaultConnectionOptions wsApp backupApp)
    where
        wsApp :: ServerApp
        wsApp pending_conn = do
            conn <- acceptRequest pending_conn
            putStrLn "new client"
            sendTextData conn ("Hello, client!" :: Text)
            sendClose conn ("Connection closed" :: Text)

        backupApp :: Application
        backupApp _ respond = do
            respond $ responseLBS status400 [] "Not a WebSocket request"
