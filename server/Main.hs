{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import API
import ServerSide

import Servant
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import System.Environment


server :: Server ServerAPI
server = serveGame :<|> serveService

app :: Application
app = serve (Proxy :: Proxy ServerAPI) server

main :: IO()
main = do
    argv <- getArgs
    let http_port = (read . head) argv :: Int

    run http_port app