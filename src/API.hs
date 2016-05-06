{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API
(
    serveGame,
    ServerAPI,
    GameAPI
) where

import Types
import Game

import Servant
import Control.Monad.Trans.Except
--import Network.WebSockets


type ServantResponse a = ExceptT ServantErr IO a


type ServerAPI
    = "game" :> GameAPI


type GameAPI
    = "new" :> Get '[JSON] GameId
    :<|> "save" :> Get '[JSON] GameId


serveGame :: Server GameAPI
serveGame = newGame :<|> saveGame

newGame :: ServantResponse GameId
newGame = return 1

saveGame :: ServantResponse GameId
saveGame = return 0
