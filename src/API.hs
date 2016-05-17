{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Types
import Game
-- import ServerSide

import Servant


type ServerAPI
    = "game" :> GameAPI
    :<|> "service" :> ServiceAPI


type ServiceAPI
    = "open_socket" :> Raw


type GameAPI
    = "new" :> Get '[JSON] GameState
    :<|> "save" :> Get '[JSON] GameId
