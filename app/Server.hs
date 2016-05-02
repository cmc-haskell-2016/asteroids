{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import ServerAPI

import Servant
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)


server :: Server ServerAPI
server = serveGame

app :: Application
app = serve (Proxy :: Proxy ServerAPI) server

main :: IO()
main = run 8080 app