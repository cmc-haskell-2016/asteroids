{-# LANGUAGE RecordWildCards #-}
module Main (main) where

--import Interface
--import Rendering
import Game
import Types
import API

import Servant
import Servant.Client
import Network.HTTP.Client hiding (Proxy)
import Control.Monad.Trans.Except
import System.Environment
--import Graphics.Gloss.Interface.Pure.Game


{-
background :: Color
background = makeColorI 25 25 112 0

fps :: Int
fps = 60
-}


type ServantResponse a = ExceptT ServantError IO a


data ClientAPI = ClientAPI {
    new :: ServantResponse GameId,
    save :: ServantResponse GameId
}


serverUrl :: Ip -> Port -> BaseUrl
serverUrl ip port = BaseUrl Http ip port ""


mkAPIClient :: BaseUrl -> Manager -> Client ServerAPI
mkAPIClient = client (Proxy :: Proxy ServerAPI)


mkAsteroidsClient :: Ip -> Port -> Manager -> IO ClientAPI
mkAsteroidsClient ip port manager = return ClientAPI{..}
  where
    gameClient = mkAPIClient (serverUrl ip port) manager

    (new :<|> save) = gameClient


mkClientWithManager :: Ip -> Port -> IO ClientAPI
mkClientWithManager ip port = do
    manager <- newManager defaultManagerSettings
    mkAsteroidsClient ip port manager


main :: IO ()
main = do
    argv <- getArgs
    let ip = head argv
    let http_port = (read . head . tail) argv :: Int
    let ws_port = (read . head . tail . tail) argv :: Int
    c <- mkClientWithManager ip http_port
    res <- runExceptT (save c)
    print res
--main = play window background fps initGame renderPic handleKeys updateGame
