{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import API
import ServerSide
import Game
import Universe

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

    shared <- newTVarIO $ ServerState (InGame initUniverse) []
    _ <- forkIO $ periodicUpdates shared
    run http_port (app shared)
