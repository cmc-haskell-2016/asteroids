{-# LANGUAGE DeriveGeneric #-}

module Bullet
(
    Bullet(..),
    initBullet
) where

import Types
import Ship

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.WebSockets
import GHC.Generics (Generic)

bulletSpeed :: Float
bulletSpeed = 300

data Bullet = Bullet {
    bulLoc :: Position,
    bulAng :: Degree,
    bulRad :: Radius,
    bulVel :: Speed,
    bulAlive :: Bool,
    bulColor :: Int
} deriving (Show, Eq, Read, Generic)

instance ToJSON Bullet
instance FromJSON Bullet

instance WebSocketsData Bullet where
    fromLazyByteString = read . BL8.unpack
    toLazyByteString   = BL8.pack . show


initBullet :: Ship -> Bullet
initBullet s =
    Bullet {
        bulLoc = shipLoc s,
        bulRad = 3,
        bulAng = shipAng s,
        bulAlive = True,
        bulVel = velang,
        bulColor = shipColor s
    }
    where
        yvel = cos ((shipAng s) * pi / 180)
        xvel = sin ((shipAng s) * pi / 180)
        norm = sqrt (xvel * xvel + yvel * yvel)
        velang = (xvel /norm * bulletSpeed, yvel /norm * bulletSpeed)
