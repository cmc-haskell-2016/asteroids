{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import API
import ServerSide
import Game
import Universe

import Control.Concurrent.STM
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets
import Servant
import System.Environment


server :: TVar ServerState -> Server ServerAPI
server ss = serveGame :<|> (serveService ss)

app :: TVar ServerState -> Application
app ss = serve (Proxy :: Proxy ServerAPI) (server ss)

main :: IO()
main = do
    argv <- getArgs
    let http_port = (read . head) argv :: Int

    shared <- newTVarIO $ ServerState (InGame initUniverse) []
    forkIO $ periodicUpdates shared
    run http_port (app shared)
