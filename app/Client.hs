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


serverUrl :: BaseUrl
serverUrl = BaseUrl Http "localhost" 8080 ""


mkAPIClient :: BaseUrl -> Manager -> Client ServerAPI
mkAPIClient = client (Proxy :: Proxy ServerAPI)


mkClientWithManager :: IO ClientAPI
mkClientWithManager = do
    manager <- newManager defaultManagerSettings
    mkAsteroidsClient manager


mkAsteroidsClient :: Manager -> IO ClientAPI
mkAsteroidsClient manager = return ClientAPI{..}
  where
    gameClient = mkAPIClient serverUrl manager

    (new :<|> save) = gameClient


main = do
    c <- mkClientWithManager
    res <- runExceptT (save c)
    print res
--main = play window background fps initGame renderPic handleKeys updateGame
