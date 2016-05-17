{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Game
import Types
import API
import Rendering
import Interface

import Control.Monad (forever)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.Except
import qualified Data.Text.IO as T
import Data.ByteString.Lazy.Internal (ByteString)
import Graphics.Gloss.Interface.IO.Game
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Media.MediaType (MediaType)
import qualified Network.HTTP.Types.Header as H
import Network.HTTP.Types.Method
import qualified Network.WebSockets as WS
import Servant
import Servant.Client
import System.Environment
import System.Exit


type ServantResponse a = ExceptT ServantError IO a

background :: Color
background = makeColorI 25 25 112 0

fps :: Int
fps = 60

data ClientAPI = ClientAPI {
    new :: ServantResponse GameState,
    save :: ServantResponse GameId,
    open_socket :: Method -> ServantResponse (Int, ByteString, MediaType, [H.Header], Response ByteString)
}

data ClientState = ClientState {
    game :: TVar GameState,
    conn :: WS.Connection
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


getUpdatedGameState :: Time -> GameState -> IO GameState
getUpdatedGameState _ GameOver = return GameOver
getUpdatedGameState _ g = return g
    --get gamestate from shared memory


handleUpdates :: ClientState -> IO ()
handleUpdates state@ClientState{..} = do
    _ <- forkIO $ forever $ do
        -- msg <- WS.receiveData conn
        -- T.putStrLn msg
        game_state <- WS.receiveData conn
        atomically $ writeTVar game game_state

    return ()


main :: IO ()
main = do
    argv <- getArgs
    let ip = head argv
    let http_port = (read . head . tail) argv :: Int

    c <- mkClientWithManager ip http_port
    response <- runExceptT (new c)
    new_game <- getServerResponse response

    shared <- atomically $ newTVar new_game

    WS.runClient ip http_port "/service/open_socket" $ \conn -> do
        putStrLn "Connection successful"
        _ <- forkIO (handleUpdates (ClientState shared conn))
        playIO window background fps new_game renderPic handleKeys getUpdatedGameState
    putStrLn "Finished"
