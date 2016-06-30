{-# LANGUAGE DeriveGeneric #-}

module Universe where

import Ship
import Bullet
import Asteroid
import Types

import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.WebSockets


data Universe = Universe {
    step :: Step,
    ships :: [Ship],
    asteroids :: [Asteroid],
    bullets :: [Bullet]
} deriving (Show, Read, Generic)

instance ToJSON Universe
instance FromJSON Universe

instance WebSocketsData Universe where
    fromLazyByteString = read . BL8.unpack
    toLazyByteString   = BL8.pack . show


initUniverse :: Int -> Universe
initUniverse k = Universe {
    step = 0,
    ships = loop k,
    asteroids = [],
    bullets = []
}
    where
        loop n
            | n == 0 = []
            | otherwise = (initShip n) : (loop (n - 1))
