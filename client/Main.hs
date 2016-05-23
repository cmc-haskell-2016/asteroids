module Main (main) where

import Game
import Types
import Rendering
import Interface
import ClientSide

import Control.Monad.Trans.Except
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Data.Text.IO as T
import Graphics.Gloss.Interface.IO.Game
import qualified Network.WebSockets as WS
import System.Environment


background :: Color
background = makeColorI 25 25 112 0

fps :: Int
fps = 60


main :: IO ()
main = do
    argv <- getArgs
    let ip = head argv
    let http_port = (read . head . tail) argv :: Int

    c <- mkClientWithManager ip http_port
    response <- runExceptT (new c)
    new_game <- getServerResponse response

    shared <- newTVarIO new_game

    WS.runClient ip http_port "/service/open_socket" $ \conn -> do
        let new_state = (ClientState shared conn c)
        putStrLn "Connection successful"
        _ <- forkIO (handleUpdates new_state)
        playIO window background fps new_state renderPicIO handleKeys getUpdatedGameState
    putStrLn "Finished"

    where
