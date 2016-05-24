{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import API
import ServerSide
import Game

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVarIO, TVar)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Environment (getArgs)


server :: TVar ServerState -> Server ServerAPI
server ss = (serveGame ss) :<|> (serveService ss)

app :: TVar ServerState -> Application
app ss = serve (Proxy :: Proxy ServerAPI) (server ss)

main :: IO()
main = do
    argv <- getArgs
    let http_port = (read . head) argv :: Int
    let game_mode = (head . tail) argv
    mode <- modeHandler game_mode

    shared <- newTVarIO $ ServerState mode []
    _ <- forkIO $ periodicUpdates shared
    run http_port (app shared)
    where
        modeHandler :: String -> IO GameMode
        modeHandler "single" = return $ Single $ Waiting 1
        modeHandler "cooperative" = return $ Cooperative $ Waiting 2
        modeHandler _ = do
            putStrLn "Unknown mode, using single game by default"
            return $ Single $ Waiting 1
