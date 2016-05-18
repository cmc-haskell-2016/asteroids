module ClientSide where

import Game

import Control.Concurrent.STM
import qualified Network.WebSockets as WS


data ClientState = ClientState {
    game :: TVar GameState,
    conn :: WS.Connection
}
