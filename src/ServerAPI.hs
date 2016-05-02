{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ServerAPI where

import Types

import Servant
import Control.Monad.Trans.Except
--import Network.WebSockets


type ServantResponse a = ExceptT ServantErr IO a

type ServerAPI = "game" :> GameAPI

type GameAPI
    = "create" :> Get '[JSON] GameId
    :<|> "save" :> Get '[JSON] GameId


serveGame :: Server GameAPI
serveGame = createGame :<|> saveGame

createGame :: ServantResponse GameId
createGame = return 1

saveGame :: ServantResponse GameId
saveGame = return 0
